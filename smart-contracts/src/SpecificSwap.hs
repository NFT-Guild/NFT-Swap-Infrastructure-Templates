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
import           Prelude              (Show)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-------------------
-- ON CHAIN CODE --
-------------------

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
    | action == 0 = traceIfFalse "SWAP FAILED: No desired token sent to pool OR no desired token requested from pool" (anyDesiredTokenRequested && anyDesiredTokenReceived)
    | action == 1 = traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | otherwise   = traceError   "UNSUPPORTED ACTION"
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        anyDesiredTokenRequested :: Bool
        anyDesiredTokenRequested = (desiredPolicyID scriptParams) `elem` symbols (valueSpent info)  

        -- check if the transaction was signed by the contract owner
        performedByContractOwner :: Bool
        performedByContractOwner = txSignedBy info $ unPaymentPubKeyHash $ contractOwner scriptParams

        policyIDInValue :: CurrencySymbol -> Value -> Bool
        policyIDInValue c v = c `elem` symbols v

        -- check all received output tupple pairs to see if the currencysymbol is found
        desiredReceived :: CurrencySymbol -> [(DatumHash, Value)] -> Bool
        desiredReceived _ []        = False
        desiredReceived cs (o : os) = (policyIDInValue cs (snd o)) || desiredReceived cs os  

        anyDesiredTokenReceived :: Bool
        anyDesiredTokenReceived = desiredReceived (desiredPolicyID scriptParams) (scriptOutputsAt (ownHash ctx) info)

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
