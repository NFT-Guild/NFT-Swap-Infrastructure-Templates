{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module RandomSwapQueue where

import           Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            Validator, mkValidatorScript, TxInfo)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (applyCode, compile, liftCode, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData))
import           PlutusTx.Prelude          (Bool, ($), traceIfFalse, (.)) --, traceIfFalse, ($), (.))
import           Prelude                   (IO, Show (show))
import           Utilities                 (wrapValidator, writeValidatorToFile, writeCodeToFile)
import           Text.Printf               (printf)


---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------
{-# INLINABLE mkRandomSwapQueueValidator #-}
mkRandomSwapQueueValidator :: PubKeyHash -> () -> () -> ScriptContext -> Bool
mkRandomSwapQueueValidator hash () () ctx =
    traceIfFalse "only owner can spend resources from queue" signedByOwner
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info hash

{-# INLINABLE  mkWrappedRandomSwapQueueValidator #-}
mkWrappedRandomSwapQueueValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedRandomSwapQueueValidator pkh = wrapValidator . mkRandomSwapQueueValidator $ PlutusTx.unsafeFromBuiltinData pkh

validatorWOParameters :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorWOParameters = $$(PlutusTx.compile [|| mkWrappedRandomSwapQueueValidator ||])

validatorWParameters :: PubKeyHash -> Validator
validatorWParameters pkh = mkValidatorScript $ validatorWOParameters `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
saveValidatorWOParameters :: IO()
saveValidatorWOParameters = writeCodeToFile "assets/queue_wo_parameters.plutus" validatorWOParameters

saveValidatorWParameters :: PubKeyHash -> IO ()
saveValidatorWParameters pkh = writeValidatorToFile (printf "assets/pmd-%s.plutus" $ show pkh) $ validatorWParameters pkh

