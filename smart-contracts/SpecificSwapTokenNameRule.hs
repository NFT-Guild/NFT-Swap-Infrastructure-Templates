{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module SpecificSwapTokenNameRule where

import Plutus.V2.Ledger.Api      (PubKeyHash, ScriptContext (scriptContextTxInfo),
                                  Validator, mkValidatorScript, CurrencySymbol, Value, adaSymbol, adaToken, singleton, OutputDatum )
import Plutus.V2.Ledger.Contexts (scriptOutputsAt, ownHash, txSignedBy, TxInInfo (txInInfoResolved), TxOut (txOutValue, txOutAddress), TxInfo (..))
import PlutusTx                  (applyCode, compile, liftCode, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData), makeLift, unstableMakeIsData)
import PlutusTx.Prelude          (Bool, ($), traceIfFalse, traceError, otherwise, (==), (.), length, elem, (+), (-), filter, (&&), (>), (<), (>=), (<=), (*))
import Prelude                   (IO, all, Bool (True), (<>), Show (show))
import Utilities                 (wrapValidator, writeCodeToFile, writeValidatorToFile)
import Plutus.V1.Ledger.Value    (flattenValue, TokenName (unTokenName), symbols)
import Plutus.V1.Ledger.Address  (scriptHashAddress)
import Text.Printf               (printf)
import PlutusTx.Builtins

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Contract parameter object to save state about the contract owner, desired policy and regular expression describing allowed token names
data ContractParam = ContractParam
    { contractOwner   :: !PubKeyHash
    , desiredPolicyID :: !CurrencySymbol
    , tnStartsWith    :: !BuiltinByteString
    , tnNumStartIndex :: !Integer
    , tnNumLength     :: !Integer
    , tnNumStartRange :: !Integer
    , tnNumEndRange   :: !Integer
    }

PlutusTx.makeLift ''ContractParam
PlutusTx.unstableMakeIsData ''ContractParam


{-# INLINABLE mkNFTSwapValidator #-}
--                    Parameter to script      Datum     Redeemer     ScriptContext    Result
mkNFTSwapValidator :: ContractParam         -> ()     -> Integer   -> ScriptContext -> Bool
mkNFTSwapValidator scriptParams _ action ctx
     | action == 0 = let tokensRequested'          = tokensRequested
                         tokensReceived'           = tokensReceived
                         numRequested              = numDesiredTokensRequested tokensRequested'
                         numReceived               = numDesiredTokensReceived tokensReceived'
                         requestedAndReceivedEqual = traceIfFalse "Number of tokens requested not equal to number of tokens received" $ numRequested == numReceived
                         noUnlistedTokensWithdrawn = traceIfFalse "Unlisted tokens can only be withdrawn by the contract owner"       $ numUnlistedTokensRequested tokensRequested' == 0
                     in                              traceIfFalse "SWAP FAILED"                                                       $ all (==True) [ requestedAndReceivedEqual
                                                                                                                                                    , noUnlistedTokensWithdrawn
                                                                                                                                                    ]
    | action == 1 = traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | otherwise   = traceError   "UNSUPPORTED ACTION"
    where

        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- return the inputs that originate from the script
        utxosSpentFromSc :: [TxInInfo]
        utxosSpentFromSc = filter (\u -> txOutAddress (txInInfoResolved u) == scriptHashAddress (ownHash ctx)) $ txInfoInputs info

        -- return the Value contained in the tx inputs list
        getTxInValueOnly :: [TxInInfo] -> Value
        getTxInValueOnly []             = singleton adaSymbol adaToken 0
        getTxInValueOnly (i : is)       = txOutValue (Plutus.V2.Ledger.Contexts.txInInfoResolved i) <> getTxInValueOnly is

        -- number of tokens requested that are members of the desired policy id(s) and optionally the TokenName list
        numDesiredTokensRequested :: Value -> Integer
        numDesiredTokensRequested requestedTokens = let desiredCS        = desiredPolicyID scriptParams
                                                    in  numSwappableTokens (flattenValue requestedTokens) desiredCS

        -- number of tokens requested that are not from the desired policy id(s)
        numUnlistedTokensRequested :: Value -> Integer
        numUnlistedTokensRequested requestedTokens = let valuedCSs = [desiredPolicyID scriptParams, adaSymbol]
                                                         producedCSs = symbols requestedTokens
                                                     in  length (removeCurrencySymbolsFromList valuedCSs producedCSs)

        -- remove all CurrencySymbols of list 1 from list 2
        removeCurrencySymbolsFromList :: [CurrencySymbol] -> [CurrencySymbol] -> [CurrencySymbol]
        removeCurrencySymbolsFromList [] cs = cs
        removeCurrencySymbolsFromList _  [] = []
        removeCurrencySymbolsFromList csToRemove (c : cs) = if c `elem` csToRemove
                                                            then removeCurrencySymbolsFromList csToRemove cs
                                                            else c : removeCurrencySymbolsFromList csToRemove cs

        tokensRequested :: Value
        tokensRequested = getTxInValueOnly utxosSpentFromSc

        tokensReceived :: Value
        tokensReceived = let scriptTxOuts = scriptOutputsAt (ownHash ctx) info
                         in getTxOutValueOnly scriptTxOuts

        -- return the number of desired tokens sent to the smart contract
        numDesiredTokensReceived :: Value -> Integer
        numDesiredTokensReceived receivedTokens = let desiredCS    = desiredPolicyID scriptParams
                                                  in  numSwappableTokens (flattenValue receivedTokens) desiredCS

        -- check if the transaction was signed by the contract owner
        performedByContractOwner :: Bool
        performedByContractOwner = txSignedBy info $ contractOwner scriptParams

        -- create Value from TxOut
        getTxOutValueOnly :: [(OutputDatum, Value)] -> Value
        getTxOutValueOnly []             = singleton adaSymbol adaToken 0
        getTxOutValueOnly ((_, v) : tos) = v <> getTxOutValueOnly tos

        -- count the number of tokens that have a specific policy id and name contained in allowed token name list
        numSwappableTokens :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> Integer
        numSwappableTokens [] _ = 0
        numSwappableTokens ((vc, vt, vi) : vs) cs = let bsTokenName = unTokenName vt
                                                        bTokenNameStartsWith = isPrefixOfByteString (tnStartsWith scriptParams) bsTokenName
                                                        bsNftNum = sliceByteString (tnNumStartIndex scriptParams) (tnNumLength scriptParams) bsTokenName
                                                        iNftNum = convertByteNumToInteger bsNftNum
                                                    in  if all (==True)
                                                          [ vc == cs
                                                          , bTokenNameStartsWith
                                                          , iNftNum >= tnNumStartRange scriptParams
                                                          , iNftNum <= tnNumEndRange scriptParams
                                                          ]
                                                        then vi + numSwappableTokens vs cs
                                                        else 0  + numSwappableTokens vs cs

        isPrefixOfByteString :: BuiltinByteString -> BuiltinByteString -> Bool
        isPrefixOfByteString prefix fullString = let prefixLength = lengthOfByteString prefix
                                                     startOfFullString = sliceByteString 0 prefixLength fullString
                                                 in  equalsByteString prefix startOfFullString


        reverseByteString :: BuiltinByteString -> BuiltinByteString
        reverseByteString bs
          | lengthOfByteString bs <= 0 = emptyByteString
          | lengthOfByteString bs == 1 = bs
          | lengthOfByteString bs == 2 = consByteString (indexByteString bs 1) (sliceByteString 0 1 bs)
          | lengthOfByteString bs >= 3 = consByteString (indexByteString bs (lengthOfByteString bs - 1)) (reverseByteString (sliceByteString 0 (lengthOfByteString bs - 1) bs))


        convertByteNumToInteger :: BuiltinByteString -> Integer
        convertByteNumToInteger bsNum = longerByteStringToInteger (reverseByteString bsNum) 0

        longerByteStringToInteger :: BuiltinByteString -> Integer -> Integer
        longerByteStringToInteger bs ex
            | lengthOfByteString bs  == 0 = 0
            | lengthOfByteString bs >= 1 = (byteToInteger (indexByteString bs 0) * (10 `pow` ex)) + longerByteStringToInteger (sliceByteString 1 (lengthOfByteString bs - 1) bs) (ex + 1)


        byteToInteger :: Integer -> Integer
        byteToInteger i = if i > 47 && i < 58
                           then i - 48
                           else -1

        -- calculate the power of a positive exponent 
        pow :: Integer -> Integer -> Integer
        pow original exp
          | exp == 0 = 1
          | exp == 1 = original
          | otherwise = original * (original `pow` (exp - 1))

{-# INLINABLE  mkWrappedNFTSwapValidator #-}
mkWrappedNFTSwapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTSwapValidator scriptParams = wrapValidator . mkNFTSwapValidator $ PlutusTx.unsafeFromBuiltinData scriptParams

validatorWOParameters :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorWOParameters = $$(PlutusTx.compile [|| mkWrappedNFTSwapValidator ||])

validatorWParameters :: ContractParam -> Validator
validatorWParameters scriptParams = mkValidatorScript $ validatorWOParameters `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData scriptParams)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveRuleValidatorWOParameters :: IO()
saveRuleValidatorWOParameters = writeCodeToFile "assets/specific-swap-tn-rule-uninitialized.plutus" validatorWOParameters

saveRuleValidatorWParameters :: ContractParam -> IO ()
saveRuleValidatorWParameters scriptParams = writeValidatorToFile (printf "assets/specific-swap-tn-rule-%s.plutus" $ show (contractOwner scriptParams)) $ validatorWParameters scriptParams

