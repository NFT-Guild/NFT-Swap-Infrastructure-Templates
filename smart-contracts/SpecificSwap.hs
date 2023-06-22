{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module SpecificSwap where

import Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash, ScriptContext (scriptContextTxInfo),
                                  Validator, mkValidatorScript, CurrencySymbol, Value, adaSymbol, adaToken, singleton, OutputDatum )
import Plutus.V2.Ledger.Contexts (scriptOutputsAt, ownHash, txSignedBy, TxInInfo (txInInfoResolved), TxOut (txOutValue, txOutAddress), TxInfo (..))
import PlutusTx                  (applyCode, compile, liftCode, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData), makeLift, unstableMakeIsData)
import PlutusTx.Prelude          (Bool, ($), traceIfFalse, traceError, otherwise, (==), (.), Integer, length, elem, (+), filter, (>))
import Prelude                   (IO, all, Bool (True), (<>), Show (show))
import Utilities                 (wrapValidator, writeCodeToFile, writeValidatorToFile)
import Plutus.V1.Ledger.Value    (flattenValue, TokenName, symbols)
import Plutus.V1.Ledger.Address  (scriptHashAddress)
import Text.Printf               (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Contract parameter object to save state about the contract owner and desired policy
data ContractParam = ContractParam
    { contractOwner   :: !PubKeyHash
    , desiredPolicyID :: !CurrencySymbol
    }
PlutusTx.makeLift ''ContractParam
PlutusTx.unstableMakeIsData ''ContractParam


{-# INLINABLE mkNFTSwapValidator #-}
--                    Parameter to script      Datum     Redeemer     ScriptContext    Result
mkNFTSwapValidator :: ContractParam         -> ()     -> Integer   -> ScriptContext -> Bool
mkNFTSwapValidator scriptParams _ action ctx
     | action == 0 = let numRequested          = numDesiredTokensRequested
                         numReceived           = numDesiredTokensReceived
                         requestedAndReceivedEqual = traceIfFalse "Number of tokens requested not equal to number of tokens received" $ numRequested == numReceived
                         atLeastOneTokenSwapped    = traceIfFalse "At least one token is required for swap operation to be performed" $ numReceived > 0
                         noUnlistedTokensWithdrawn = traceIfFalse "Unlisted tokens can only be withdrawn by the contract owner"       $ numUnlistedTokensRequested == 0
                     in                              traceIfFalse "SWAP FAILED"                                                       $ all (==True) [ requestedAndReceivedEqual
                                                                                                                                                     , atLeastOneTokenSwapped
                                                                                                                                                     , noUnlistedTokensWithdrawn
                                                                                                                                                     ]
    | action == 1 = traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | otherwise   = traceError   "UNSUPPORTED ACTION"
    where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- check if the transaction was signed by the contract owner
    performedByContractOwner :: Bool
    performedByContractOwner = txSignedBy info $ contractOwner scriptParams

    -- return the number of desired tokens requested from the smart contract
    numDesiredTokensRequested :: Integer
    numDesiredTokensRequested = let desiredCS        = desiredPolicyID scriptParams
                                    valueSpentFromSc = tokensRequested
                                in  numOfTokensFromPolicy (flattenValue valueSpentFromSc) desiredCS

    tokensRequested :: Value
    tokensRequested = getTxInValueOnly utxosSpentFromSc

    -- return the Value contained in the tx inputs list
    getTxInValueOnly :: [TxInInfo] -> Value
    getTxInValueOnly []             = singleton adaSymbol adaToken 0
    getTxInValueOnly (i : is)       = txOutValue (Plutus.V2.Ledger.Contexts.txInInfoResolved i) <> (getTxInValueOnly is)

    -- create Value from TxOut
    getTxOutValueOnly :: [(OutputDatum, Value)] -> Value
    getTxOutValueOnly []             = singleton adaSymbol adaToken 0
    getTxOutValueOnly ((_, v) : tos) = v <> (getTxOutValueOnly tos)

    -- return the inputs that originate from the script
    utxosSpentFromSc :: [TxInInfo]
    utxosSpentFromSc = filter (\u -> (txOutAddress (txInInfoResolved u)) == scriptHashAddress (ownHash ctx)) $ txInfoInputs info

    -- count number of tokens in flatten value that have a specific CurrencySymbol
    numOfTokensFromPolicy :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> Integer
    numOfTokensFromPolicy [] _ = 0
    numOfTokensFromPolicy ((vc, _, vi) : vs) cs = if vc == cs
                                                  then vi + (numOfTokensFromPolicy vs cs)
                                                  else 0  + (numOfTokensFromPolicy vs cs)

    -- return the number of desired tokens sent to the smart contract
    numDesiredTokensReceived :: Integer
    numDesiredTokensReceived =  let desiredCS    = desiredPolicyID scriptParams
                                    scriptTxOuts = scriptOutputsAt (ownHash ctx) info
                                    txOutValues  = getTxOutValueOnly scriptTxOuts
                                in  numOfTokensFromPolicy (flattenValue txOutValues) desiredCS
    
    -- number of tokens requested that are not from the desired policy id(s)
    numUnlistedTokensRequested :: Integer
    numUnlistedTokensRequested = let valuedCSs = [(desiredPolicyID scriptParams), adaSymbol]
                                     producedCSs = symbols tokensRequested
                                 in  length (removeCurrencySymbolsFromList valuedCSs producedCSs)

    -- remove all CurrencySymbols of list 1 from list 2
    removeCurrencySymbolsFromList :: [CurrencySymbol] -> [CurrencySymbol] -> [CurrencySymbol]
    removeCurrencySymbolsFromList [] cs = cs
    removeCurrencySymbolsFromList _  [] = []
    removeCurrencySymbolsFromList csToRemove (c : cs) = if c `elem` csToRemove
                                                        then removeCurrencySymbolsFromList csToRemove cs
                                                        else c : removeCurrencySymbolsFromList csToRemove cs


{-# INLINABLE  mkWrappedNFTSwapValidator #-}
mkWrappedNFTSwapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTSwapValidator scriptParams = wrapValidator . mkNFTSwapValidator $ PlutusTx.unsafeFromBuiltinData scriptParams

validatorWOParameters :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorWOParameters = $$(PlutusTx.compile [|| mkWrappedNFTSwapValidator ||])

validatorWParameters :: ContractParam -> Validator
validatorWParameters scriptParams = mkValidatorScript $ validatorWOParameters `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData scriptParams)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveValidatorWOParameters :: IO()
saveValidatorWOParameters = writeCodeToFile "assets/specific_swap_uninitialized.plutus" validatorWOParameters

saveValidatorWParameters :: ContractParam -> IO ()
saveValidatorWParameters scriptParams = writeValidatorToFile (printf "assets/specific-swap-%s.plutus" $ show (contractOwner scriptParams)) $ validatorWParameters scriptParams

