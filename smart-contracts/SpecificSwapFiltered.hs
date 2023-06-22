{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module SpecificSwapFiltered where

import Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash, ScriptContext (scriptContextTxInfo),
                                  Validator, mkValidatorScript, CurrencySymbol, Value, adaSymbol, adaToken, singleton, OutputDatum )
import Plutus.V2.Ledger.Contexts (scriptOutputsAt, ownHash, txSignedBy, TxInInfo (txInInfoResolved), TxOut (txOutValue, txOutAddress), TxInfo (..))
import PlutusTx                  (applyCode, compile, liftCode, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData), makeLift, unstableMakeIsData)
import PlutusTx.Prelude          (Bool, ($), traceIfFalse, traceError, otherwise, (==), (.), Integer, length, elem, (+), filter, (&&))
import Prelude                   (IO, all, Bool (True), (<>), Show (show))
import Utilities                 (wrapValidator, writeCodeToFile, writeValidatorToFile)
import Plutus.V1.Ledger.Value    (flattenValue, TokenName, symbols)
import Plutus.V1.Ledger.Address  (scriptHashAddress)
import Text.Printf               (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Contract parameter object to save state about the contract owner, desired policy and allowed token names
data ContractParam = ContractParam
    { contractOwner   :: !PubKeyHash
    , desiredPolicyID :: !CurrencySymbol
    , tokensAllowedToSwap :: ![TokenName]
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
                         noUnlistedTokensWithdrawn = traceIfFalse "Unlisted tokens can only be withdrawn by the contract owner"       $ (numUnlistedTokensRequested tokensRequested') == 0
                    in                               traceIfFalse "SWAP FAILED"                                                       $ all (==True) [ requestedAndReceivedEqual
                                                                                                                                                    , noUnlistedTokensWithdrawn
                                                                                                                                                    ]  
    | action == 1 = traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | otherwise   = traceError   "UNSUPPORTED ACTION"
    where

        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- return the inputs that originate from the script
        utxosSpentFromSc :: [TxInInfo]
        utxosSpentFromSc = filter (\u -> (txOutAddress (txInInfoResolved u)) == scriptHashAddress (ownHash ctx)) $ txInfoInputs info

        -- return the Value contained in the tx inputs list
        getTxInValueOnly :: [TxInInfo] -> Value
        getTxInValueOnly []             = singleton adaSymbol adaToken 0
        getTxInValueOnly (i : is)       = txOutValue (Plutus.V2.Ledger.Contexts.txInInfoResolved i) <> (getTxInValueOnly is)

        -- number of tokens requested that are members of the desired policy id(s) and optionally the TokenName list
        numDesiredTokensRequested :: Value -> Integer
        numDesiredTokensRequested requestedTokens = let desiredCS        = desiredPolicyID scriptParams
                                                    in  numSwappableTokens (flattenValue requestedTokens) desiredCS

        -- number of tokens requested that are not from the desired policy id(s)
        numUnlistedTokensRequested :: Value -> Integer
        numUnlistedTokensRequested requestedTokens = let valuedCSs = [(desiredPolicyID scriptParams), adaSymbol]
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
        tokensReceived = let scriptTxOuts = (scriptOutputsAt (ownHash ctx) info)
                         in getTxOutValueOnly scriptTxOuts

        -- return the number of desired tokens sent to the smart contract
        numDesiredTokensReceived :: Value -> Integer
        numDesiredTokensReceived receivedTokens = let desiredCS    = (desiredPolicyID scriptParams)
                                                  in  numSwappableTokens (flattenValue receivedTokens) desiredCS

        -- check if the transaction was signed by the contract owner
        performedByContractOwner :: Bool
        performedByContractOwner = txSignedBy info $ contractOwner scriptParams

        -- create Value from TxOut
        getTxOutValueOnly :: [(OutputDatum, Value)] -> Value
        getTxOutValueOnly []             = singleton adaSymbol adaToken 0
        getTxOutValueOnly ((_, v) : tos) = v <> (getTxOutValueOnly tos)

        -- if list is empty, all token names are swappable. If list contains names, the name must be in the list to be swappable
        isSwappableTokenName :: [TokenName] -> TokenName -> Bool
        isSwappableTokenName [] _ = True -- no list of allowed TokenNames provided. All TokenNames in CurrencySymbol are swappable
        isSwappableTokenName allowedTns tn = tn `elem` allowedTns 

        -- count the number of tokens that have a specific policy id and name contained in allowed token name list
        numSwappableTokens :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> Integer
        numSwappableTokens [] _ = 0
        numSwappableTokens ((vc, vt, vi) : vs) cs = if vc == cs && isSwappableTokenName (tokensAllowedToSwap scriptParams) vt
                                                      then vi + (numSwappableTokens vs cs)
                                                      else 0  + (numSwappableTokens vs cs)


{-# INLINABLE  mkWrappedNFTSwapValidator #-}
mkWrappedNFTSwapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTSwapValidator scriptParams = wrapValidator . mkNFTSwapValidator $ PlutusTx.unsafeFromBuiltinData scriptParams

validatorWOParameters :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorWOParameters = $$(PlutusTx.compile [|| mkWrappedNFTSwapValidator ||])

validatorWParameters :: ContractParam -> Validator
validatorWParameters scriptParams = mkValidatorScript $ validatorWOParameters `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData scriptParams)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveFilteredValidatorWOParameters :: IO()
saveFilteredValidatorWOParameters = writeCodeToFile "assets/specific-swap-filtered-uninitialized.plutus" validatorWOParameters

saveFilteredValidatorWParameters :: ContractParam -> IO ()
saveFilteredValidatorWParameters scriptParams = writeValidatorToFile (printf "assets/specific-swap-filtered-%s.plutus" $ show (contractOwner scriptParams)) $ validatorWParameters scriptParams

