{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RandomSwapQueue where

import           Plutus.V2.Ledger.Api      (BuiltinData,
                                            ScriptContext (scriptContextTxInfo),
                                            Validator, mkValidatorScript, TxInfo, PubKeyHash)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (unstableMakeIsData, applyCode, compile, liftCode, makeLift, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData))
import           PlutusTx.Prelude          (Bool, ($), traceIfFalse, traceError, Integer, error, otherwise) 
import           Prelude                   (IO, Show (show))
import           Utilities                 (writeValidatorToFile, writeCodeToFile)
import           Text.Printf               (printf)
import           PlutusTx.Builtins         (equalsInteger)

{- Random Swap Queue Contract
   This contract implements a queue mechanism for NFT swaps.
   It manages swap requests and ensures only authorized parties can process them.
-}

{- Main Contract Types -}

-- ContractParam: Configuration parameters for the queue contract
-- Contains the contract owner's public key hash and the authorized swapper
-- who can process swap requests
data ContractParam = ContractParam
    { contractOwner   :: !PubKeyHash
    , authorizedSwapper :: !PubKeyHash
    }
PlutusTx.makeLift ''ContractParam
PlutusTx.unstableMakeIsData ''ContractParam

-- RandomQueueDatum: Stores information about the swap request
-- Contains the public key hash of the user who initiated the swap
data RandomQueueDatum = RandomQueueDatum
    { swapInitiator   :: !PubKeyHash
    }
PlutusTx.makeLift ''RandomQueueDatum
PlutusTx.unstableMakeIsData ''RandomQueueDatum

-- RandomQueueRedeemer: Defines the action to be performed
-- Action 0: Process swap (authorized swapper only)
-- Action 1: Cleanup (contract owner only)
-- Action 2: Cancel swap (swap initiator only)
data RandomQueueRedeemer = RandomQueueRedeemer
    {  action :: !Integer
    }
PlutusTx.makeLift ''RandomQueueRedeemer
PlutusTx.unstableMakeIsData ''RandomQueueRedeemer


{- Main Validator Logic 
   The validator supports three actions:
   Action 0: Process swap - only authorized swapper can execute
   Action 1: Cleanup operation - restricted to contract owner
   Action 2: Cancel swap - only the swap initiator can cancel their request
-}

{-# INLINABLE mkRandomSwapQueueValidator #-}
--                            Parameter to script    Datum               Redeemer               ScriptContext    Result
mkRandomSwapQueueValidator :: ContractParam       -> RandomQueueDatum -> RandomQueueRedeemer -> ScriptContext -> Bool
mkRandomSwapQueueValidator scriptParams datum redeemer ctx
    | action redeemer `equalsInteger` 0 =
            traceIfFalse "SWAP FAILED: Swaps can only be performed by the authorized swapper " performedBySwapper
    | action redeemer `equalsInteger` 1 =
            traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | action redeemer `equalsInteger` 2 =
            traceIfFalse "CANCELLATION FAILED: Cancellations can only be performed by the swap initiator " performedBySwapInitiator
    | otherwise = traceError "UNSUPPORTED ACTION"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- performedBySwapInitiator: Verifies transaction is signed by the swap initiator
    -- Used to validate swap cancellations
    performedBySwapInitiator :: Bool
    performedBySwapInitiator = txSignedBy info (swapInitiator datum)

    -- performedBySwapper: Verifies transaction is signed by authorized swapper
    -- Used to validate swap processing
    performedBySwapper :: Bool
    performedBySwapper = txSignedBy info (authorizedSwapper scriptParams)

    -- performedByContractOwner: Verifies transaction is signed by contract owner
    -- Used to validate cleanup operations
    performedByContractOwner :: Bool
    performedByContractOwner = txSignedBy info (contractOwner scriptParams)

{- Contract Compilation and Deployment Functions -}

{-# INLINABLE mkWrappedRandomSwapQueueValidator #-}
mkWrappedRandomSwapQueueValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedRandomSwapQueueValidator scriptParams datum redeemer ctx =
    let unwrappedScriptParams = PlutusTx.unsafeFromBuiltinData scriptParams :: ContractParam
        unwrappedDatum = PlutusTx.unsafeFromBuiltinData datum :: RandomQueueDatum
        unwrappedRedeemer = PlutusTx.unsafeFromBuiltinData redeemer :: RandomQueueRedeemer
        unwrappedCtx = PlutusTx.unsafeFromBuiltinData ctx :: ScriptContext
    in if mkRandomSwapQueueValidator unwrappedScriptParams unwrappedDatum unwrappedRedeemer unwrappedCtx
       then ()
       else PlutusTx.Prelude.error ()

validatorWOParameters :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorWOParameters = $$(PlutusTx.compile [|| mkWrappedRandomSwapQueueValidator ||])

validatorWParameters :: ContractParam -> Validator
validatorWParameters scriptParams = mkValidatorScript $ validatorWOParameters `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData scriptParams)

{- Helper Functions for Contract Deployment -}

-- saveValidatorWOParameters: Saves unparameterized validator to file
saveValidatorWOParameters :: IO()
saveValidatorWOParameters = writeCodeToFile "assets/swap-queue-wo-parameters.plutus" validatorWOParameters

-- saveValidatorWParameters: Saves parameterized validator to file
-- Creates unique filename based on contract owner
saveValidatorWParameters :: ContractParam -> IO ()
saveValidatorWParameters scriptParams = writeValidatorToFile (printf "assets/swap-queue-%s.plutus" $ show (contractOwner scriptParams)) $ validatorWParameters scriptParams

