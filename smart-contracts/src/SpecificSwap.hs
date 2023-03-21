{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SpecificSwap where

import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada
import           Prelude              (Show, (<>))


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Contract parameter object to save state about contract owner
data ContractParam = ContractParam
    { contractOwner :: PaymentPubKeyHash
    , desiredPolicyID :: CurrencySymbol
    } deriving Show

-- Tell compiler the ContractParam is liftable
PlutusTx.makeLift ''ContractParam

{-# INLINABLE mkNFTSwapValidator #-}
--                    Parameter to script   Datum          Redeemer               ScriptContext     Result
mkNFTSwapValidator :: ContractParam      -> ()          -> Integer             -> ScriptContext  -> Bool
mkNFTSwapValidator scriptParams _ action ctx
    | action == 0 = traceIfFalse "SWAP FAILED: Number of tokens requested not equal to number of tokens sent" $ numDesiredTokensRequested == numDesiredTokensReceived
    | action == 1 = traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | otherwise   = traceError   "UNSUPPORTED ACTION"
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- return all values that are only in first value and not in the second (opposite of intersect)
        exceptValues :: [(CurrencySymbol, TokenName, Integer)] -> Value -> Value
        exceptValues [] _  = lovelaceValueOf 0
        exceptValues ((c,t,i) : v1s) v2 = if Value.isZero v2
                                          then lovelaceValueOf 0
                                          else
                                            if valueOf v2 c t >= 1
                                            then exceptValues v1s v2
                                            else (singleton c t i) <> exceptValues v1s v2

        -- return the inputs that originate from the script
        utxosSpentFromSc :: [TxInInfo]
        utxosSpentFromSc = filter (\u -> (txOutAddress (txInInfoResolved u)) == scriptHashAddress (ownHash ctx)) $ txInfoInputs info

        -- return the Value contained in the tx inputs list
        getTxInValueOnly :: [TxInInfo] -> Value
        getTxInValueOnly []             = lovelaceValueOf 0
        getTxInValueOnly (i : is)       = txOutValue (txInInfoResolved i) <> (getTxInValueOnly is)

        -- return the number of desired tokens requested from the smart contract
        numDesiredTokensRequested :: Integer
        numDesiredTokensRequested = let desiredCS        = desiredPolicyID scriptParams
                                        valueSpentFromSc = getTxInValueOnly utxosSpentFromSc 
                                    in  numOfTokensFromPolicy (flattenValue valueSpentFromSc) desiredCS

        -- return the number of desired tokens sent to the smart contract
        numDesiredTokensReceived :: Integer
        numDesiredTokensReceived = let desiredCS    = (desiredPolicyID scriptParams)
                                       scriptTxOuts = (scriptOutputsAt (ownHash ctx) info)
                                       txOutValues  = getTxOutValueOnly scriptTxOuts 
                                   in  numOfTokensFromPolicy (flattenValue txOutValues) desiredCS

        -- check if the transaction was signed by the contract owner
        performedByContractOwner :: Bool
        performedByContractOwner = txSignedBy info $ unPaymentPubKeyHash $ contractOwner scriptParams

        -- create Value from TxOut
        getTxOutValueOnly :: [(DatumHash, Value)] -> Value
        getTxOutValueOnly []             = lovelaceValueOf 0
        getTxOutValueOnly ((_, v) : tos) = v <> (getTxOutValueOnly tos) 

        -- count number of tokens in flatten value that have a specific CurrencySymbol
        numOfTokensFromPolicy :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> Integer
        numOfTokensFromPolicy [] _ = 0
        numOfTokensFromPolicy ((vc, _, vi) : vs) cs = if vc == cs
                                                      then vi + (numOfTokensFromPolicy vs cs)
                                                      else 0  + (numOfTokensFromPolicy vs cs)


-- tell compiler the types of Datum and Redeemer
data SwapData
instance Scripts.ValidatorTypes SwapData where
    type instance DatumType SwapData = ()
    type instance RedeemerType SwapData = Integer

-- turning the mkNFTSwapValidator into compiled code
typedValidator :: ContractParam -> Scripts.TypedValidator SwapData
{- 
the typedValidator now has a parameter, so we need to apply parameter to 
validator before it is compiled. Problem is that scriptParam is not known at 
compile time, but is known at runtime. 
liftCode lets us compile scriptParams at runtime and we then apply this compiled 
code to get a compiled version of the validator with parameters applied
-}
typedValidator scriptParams = Scripts.mkTypedValidator @SwapData
    ($$(PlutusTx.compile [|| mkNFTSwapValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode scriptParams)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

validator :: ContractParam -> Validator
validator = Scripts.validatorScript . typedValidator

-- calculating the validator hash
valHash :: ContractParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

-- getting the script address
scrAddress :: ContractParam -> Ledger.Address
scrAddress = scriptAddress . validator
