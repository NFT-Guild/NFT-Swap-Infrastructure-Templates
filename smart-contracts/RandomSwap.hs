{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveAnyClass        #-}

module RandomSwap where

import Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash (..), ScriptContext (scriptContextTxInfo), TokenName,
                                  Validator, mkValidatorScript, CurrencySymbol, Value, adaSymbol, adaToken, singleton, OutputDatum (..), BuiltinByteString, TxInInfo (..), TxOut (..), Datum, unionWith, Credential (..), ValidatorHash)
import Plutus.V2.Ledger.Contexts (scriptOutputsAt, txSignedBy, TxInfo (..), ownHash)
import PlutusTx                  (applyCode, compile, liftCode, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData), unstableMakeIsData, fromBuiltinData, makeLift, makeIsDataIndexed)
import PlutusTx.Prelude          (Bool (True, False), ($), traceIfFalse, traceError, otherwise, Integer, length, elem, (+), filter, mapMaybe, Maybe (..), lengthOfByteString, indexByteString, appendByteString, sliceByteString, (&&), emptyByteString, consByteString, (==), null, head, error, (/=), Monoid, foldr, mempty, mappend, (.), zero, foldl, any, (-), (*))
import Prelude                   (IO, (<>), Show (show), not)
import Utilities                 (writeCodeToFile, writeValidatorToFile)
import Plutus.V1.Ledger.Value    (flattenValue, symbols, valueOf)
import Plutus.V1.Ledger.Address  (scriptHashAddress, Address(..))
import Text.Printf               (printf)
import PlutusTx.Builtins         (equalsInteger, greaterThanInteger, modInteger, divideInteger, subtractInteger, lessThanEqualsInteger, equalsByteString, addInteger, sha2_256)


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- Main Contract Types 
   This contract implements a random NFT swap mechanism with external randomness source.
   It supports NFT exchanges based on randomized selection and cleanup operations.
-}

-- ContractParam: Configuration parameters for the swap contract
-- Contains the contract owner's public key hash, the policy ID for acceptable NFTs,
-- and an authorized transaction signer for additional security
data ContractParam = ContractParam
    { contractOwner   :: !Plutus.V2.Ledger.Api.PubKeyHash
    , desiredPolicyID :: !Plutus.V2.Ledger.Api.CurrencySymbol
    , authorizedTxSigner :: !Plutus.V2.Ledger.Api.PubKeyHash
    }
PlutusTx.makeLift ''ContractParam
PlutusTx.unstableMakeIsData ''ContractParam

-- RandomnessRedeemer: Parameters used for validating random NFT selection
-- Contains random source verification data, target values for selection,
-- position calculations for byte selection from NFT names, and action identifier
data RandomnessRedeemer = RandomnessRedeemer
    { randomString :: !Plutus.V2.Ledger.Api.BuiltinByteString
    , randomIntegerHex :: !Plutus.V2.Ledger.Api.BuiltinByteString
    , targetValue :: !Integer
    , slot :: !Integer
    , pos1 :: !Integer
    , pos2 :: !Integer
    , selectionValue :: !Integer
    , diff :: !Integer
    , action :: !Integer
    }
PlutusTx.makeIsDataIndexed ''RandomnessRedeemer [('RandomnessRedeemer, 0)]
PlutusTx.makeLift ''RandomnessRedeemer

-- RandomnessOracleDatum: Structure for external randomness source data
-- Contains block-related data used for randomness verification
data RandomnessOracleDatum = RandomnessOracleDatum
    { field1 :: Plutus.V2.Ledger.Api.BuiltinData
    , field2 :: Plutus.V2.Ledger.Api.BuiltinData
    , blockData ::  (Integer, Plutus.V2.Ledger.Api.BuiltinByteString, Integer)
    , field4 :: Plutus.V2.Ledger.Api.BuiltinData
    , field5 :: Plutus.V2.Ledger.Api.BuiltinData
    , field6 :: Plutus.V2.Ledger.Api.BuiltinData
    }
PlutusTx.makeLift ''RandomnessOracleDatum
PlutusTx.makeIsDataIndexed ''RandomnessOracleDatum [('RandomnessOracleDatum, 0)]

{- Main Validator Logic 
   The validator supports two actions:
   Action 0: Random NFT swap - validates both NFT exchange and randomness
   Action 1: Cleanup operation - restricted to contract owner
-}
{-# INLINABLE mkNFTSwapValidator #-}
--                    Parameter to script      Datum     Redeemer                ScriptContext    Result
mkNFTSwapValidator :: ContractParam         -> ()     -> RandomnessRedeemer   -> Plutus.V2.Ledger.Api.ScriptContext -> Bool
mkNFTSwapValidator scriptParams _ redeemer ctx 
    | action redeemer == 0 =
        let datumValue = fromJust (findDatum ctx)
            (_, datumBlockHash, _) = blockData datumValue
            calculatedHash = validateAndHashSlot (slot redeemer) datumBlockHash
            _ = case getNFTNameFromInput ctx of
                Nothing -> error ()
                Just name -> name
            actualDiff = if greaterThanInteger (selectionValue redeemer) (targetValue redeemer)
                        then subtractInteger (selectionValue redeemer) (targetValue redeemer)
                        else subtractInteger (targetValue redeemer) (selectionValue redeemer)
            expectedTarget = modInteger (fourBytesToInteger (randomIntegerHex redeemer)) 65536
            -- NFT exchange validations
            numRequested = numDesiredTokensRequested
            numReceived = numDesiredTokensReceived
        in  
            -- First validate NFT exchange
            traceIfFalse "Number of tokens requested not equal to number of tokens received" 
                (equalsInteger numRequested numReceived) &&
            traceIfFalse "Exactly one token must be swapped" 
                (equalsInteger numReceived 1) &&
            traceIfFalse "Unlisted tokens can only be withdrawn by the contract owner" 
                (equalsInteger numUnlistedTokensRequested 0) &&
            -- Then validate randomness selection
            traceIfFalse "Hash validation failed" 
                (equalsByteString calculatedHash (randomString redeemer)) &&
            traceIfFalse "Random integer hex doesn't match random string" 
                (equalsByteString 
                    (sliceByteString 0 4 (randomString redeemer)) 
                    (randomIntegerHex redeemer)
                ) &&
            traceIfFalse "Target value not correctly derived from random hex"
                (equalsInteger expectedTarget (targetValue redeemer)) &&
            traceIfFalse "Difference calculation incorrect" 
                (equalsInteger actualDiff (diff redeemer))
    | action redeemer `equalsInteger` 1 =
            traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | otherwise = traceError "UNSUPPORTED ACTION"

    where
        info :: TxInfo
        info = Plutus.V2.Ledger.Api.scriptContextTxInfo ctx

        -- validateAndHashSlot: Creates a hash from slot number and block hash
        -- Used to verify the randomness source validity
        validateAndHashSlot :: Integer -> Plutus.V2.Ledger.Api.BuiltinByteString -> Plutus.V2.Ledger.Api.BuiltinByteString
        validateAndHashSlot slt blockHash = 
            let slotHex = integerToHex slt
                padLen = subtractInteger 4 (lengthOfByteString slotHex)
                paddedSlot = if lessThanEqualsInteger padLen 0
                    then slotHex
                    else appendByteString (repeatByteString padLen 0) slotHex
            in sha2_256 (appendByteString paddedSlot blockHash)

        -- fourBytesToInteger: Converts 4 bytes into a single integer value
        -- Used in random number generation and verification
        fourBytesToInteger :: Plutus.V2.Ledger.Api.BuiltinByteString -> Integer
        fourBytesToInteger bs =
            let b0 = indexByteString bs 0
                b1 = indexByteString bs 1
                b2 = indexByteString bs 2
                b3 = indexByteString bs 3
            in b0 * 16777216 + b1 * 65536 + b2 * 256 + b3

        repeatByteString :: Integer -> Integer -> Plutus.V2.Ledger.Api.BuiltinByteString
        repeatByteString n byte
            | lessThanEqualsInteger n 0 = emptyByteString
            | otherwise = consByteString byte (repeatByteString (subtractInteger n 1) byte)

        -- getSpentTokens: Tracks tokens being spent from the script
        -- Filters for relevant token movements from script UTXOs
        getSpentTokens :: Plutus.V2.Ledger.Api.ScriptContext -> Plutus.V2.Ledger.Api.Value
        getSpentTokens context =
            let ownScriptHash = ownHash context
                txInfo = Plutus.V2.Ledger.Api.scriptContextTxInfo context
                isOwnInput txIn = case Plutus.V2.Ledger.Api.txOutAddress (Plutus.V2.Ledger.Api.txInInfoResolved txIn) of
                                    Address (Plutus.V2.Ledger.Api.ScriptCredential vh) _ -> vh == ownScriptHash
                                    _ -> False
                ownInputs = filter isOwnInput (txInfoInputs txInfo)
                inputValue = foldl (\acc txIn -> Plutus.V2.Ledger.Api.unionWith (+) acc (Plutus.V2.Ledger.Api.txOutValue (Plutus.V2.Ledger.Api.txInInfoResolved txIn))) zero ownInputs
                outputValue = foldl (\acc txOut -> Plutus.V2.Ledger.Api.unionWith (+) acc (Plutus.V2.Ledger.Api.txOutValue txOut)) zero (filter (\txOut -> Plutus.V2.Ledger.Api.txOutAddress txOut == Address (Plutus.V2.Ledger.Api.ScriptCredential ownScriptHash) Nothing) (txInfoOutputs txInfo))
                spentValue = subtractValue inputValue outputValue
            in filterValue (\cs tn -> not (isValueSentToContract cs tn txInfo ownScriptHash)) spentValue

        getNFTNameFromInput :: Plutus.V2.Ledger.Api.ScriptContext -> Maybe Plutus.V2.Ledger.Api.TokenName
        getNFTNameFromInput context =
            let spentTokens = getSpentTokens context
                nonAdaTokens = filterValue (\cs _ -> cs /= Plutus.V2.Ledger.Api.adaSymbol) spentTokens
            in case flattenValue nonAdaTokens of
                [] -> Nothing
                ((cs, tn, amt):_) -> if amt `greaterThanInteger` 0 && isNFTFromContract cs tn context then Just tn else Nothing

        -- isNFTFromContract: Verifies NFT ownership by the contract
        -- Ensures only valid NFTs can be swapped
        isNFTFromContract :: Plutus.V2.Ledger.Api.CurrencySymbol -> Plutus.V2.Ledger.Api.TokenName -> Plutus.V2.Ledger.Api.ScriptContext -> Bool
        isNFTFromContract cs tn context =
            let ownScriptHash = ownHash context
                txInfo = Plutus.V2.Ledger.Api.scriptContextTxInfo context
                isOwnInput txIn = case Plutus.V2.Ledger.Api.txOutAddress (Plutus.V2.Ledger.Api.txInInfoResolved txIn) of
                                    Address (Plutus.V2.Ledger.Api.ScriptCredential vh) _ -> vh == ownScriptHash
                                    _ -> False
                ownInputs = filter isOwnInput (txInfoInputs txInfo)
            in any (\txIn -> valueOf (Plutus.V2.Ledger.Api.txOutValue (Plutus.V2.Ledger.Api.txInInfoResolved txIn)) cs tn `greaterThanInteger` 0) ownInputs

        -- isValueSentToContract: Tracks token movements to the contract
        -- Part of the token movement validation system
        isValueSentToContract :: Plutus.V2.Ledger.Api.CurrencySymbol -> Plutus.V2.Ledger.Api.TokenName -> TxInfo -> Plutus.V2.Ledger.Api.ValidatorHash -> Bool
        isValueSentToContract cs tn txInfo ownScriptHash =
            let nonScriptInputs = filter (\txIn -> case Plutus.V2.Ledger.Api.txOutAddress (Plutus.V2.Ledger.Api.txInInfoResolved txIn) of
                                                    Address (Plutus.V2.Ledger.Api.ScriptCredential vh) _ -> vh /= ownScriptHash
                                                    _ -> True) (txInfoInputs txInfo)
            in any (\txIn -> valueOf (Plutus.V2.Ledger.Api.txOutValue (Plutus.V2.Ledger.Api.txInInfoResolved txIn)) cs tn `greaterThanInteger` 0) nonScriptInputs

        -- Helper function to subtract Values
        subtractValue :: Plutus.V2.Ledger.Api.Value -> Plutus.V2.Ledger.Api.Value -> Plutus.V2.Ledger.Api.Value
        subtractValue v1 v2 = Plutus.V2.Ledger.Api.unionWith (-) v1 v2

        -- Helper function to filter Value
        filterValue :: (Plutus.V2.Ledger.Api.CurrencySymbol -> Plutus.V2.Ledger.Api.TokenName -> Bool) -> Plutus.V2.Ledger.Api.Value -> Plutus.V2.Ledger.Api.Value
        filterValue f v = 
            foldMap (\(cs, tn, amt) -> if f cs tn then Plutus.V2.Ledger.Api.singleton cs tn amt else zero) (flattenValue v)

        -- Helper function for foldMap if not available
        foldMap :: (Monoid m) => (a -> m) -> [a] -> m
        foldMap f = foldr (mappend . f) mempty

        fromJust :: Maybe a -> a
        fromJust (Just x) = x
        fromJust Nothing = traceError "fromJust: Nothing"
    
        -- findDatum: Extracts and deserializes the reference input datum
        -- Used to access randomness oracle data for validation
        findDatum :: Plutus.V2.Ledger.Api.ScriptContext -> Maybe RandomnessOracleDatum
        findDatum context =
            let referenceInputs = txInfoReferenceInputs $ Plutus.V2.Ledger.Api.scriptContextTxInfo context
                datums = mapMaybe getTxOutDatum referenceInputs
            in if null datums
                then Nothing
                else PlutusTx.fromBuiltinData (PlutusTx.toBuiltinData (head datums))
         where
            -- getTxOutDatum: Extracts datum from transaction output
            -- Helper function for datum extraction
            getTxOutDatum :: Plutus.V2.Ledger.Api.TxInInfo -> Maybe Plutus.V2.Ledger.Api.Datum
            getTxOutDatum (Plutus.V2.Ledger.Api.TxInInfo _ txOut) =
                if isOutputDatum (Plutus.V2.Ledger.Api.txOutDatum txOut)
                then Just (getOutputDatum (Plutus.V2.Ledger.Api.txOutDatum txOut))
                else Nothing

            -- isOutputDatum: Checks if a datum is an OutputDatum
            -- Helper function for datum validation
            isOutputDatum :: Plutus.V2.Ledger.Api.OutputDatum -> Bool
            isOutputDatum (Plutus.V2.Ledger.Api.OutputDatum _) = True
            isOutputDatum _ = False

            -- getOutputDatum: Extracts datum from OutputDatum
            -- Helper function for datum extraction
            getOutputDatum :: Plutus.V2.Ledger.Api.OutputDatum -> Plutus.V2.Ledger.Api.Datum
            getOutputDatum (Plutus.V2.Ledger.Api.OutputDatum d) = d
            getOutputDatum _ = error ()

        --{-# INLINABLE integerToHex #-}
        integerToHex :: Integer -> Plutus.V2.Ledger.Api.BuiltinByteString
        integerToHex n =
            let go m acc
                    | equalsInteger m 0 = acc
                    | otherwise = go (divideInteger m 256) (consByteString (modInteger m 256) acc)
            in go n emptyByteString

        -- performedByContractOwner: Verifies transaction signer
        -- Ensures only contract owner can perform certain operations
        performedByContractOwner :: Bool
        performedByContractOwner = txSignedBy info $ contractOwner scriptParams

        -- numDesiredTokensRequested: Calculates net movement of tokens out of contract
        -- Ensures correct counting of NFTs being withdrawn even with multiple tokens per UTxO
        numDesiredTokensRequested :: Integer
        numDesiredTokensRequested = 
            let desiredCS = desiredPolicyID scriptParams
                inputValues = getTxInValueOnly utxosSpentFromSc
                -- Find corresponding output back to same script address
                scriptOutputs = scriptOutputsAt (ownHash ctx) info
                outputValues = getTxOutValueOnly scriptOutputs
                -- For each token in input, check if it's in output
                inputTokens = flattenValue inputValues
                outputTokens = flattenValue outputValues
                -- Count tokens that were in input but not in output (or less in output)
                countMovedTokens = foldl (\acc (cs, tn, inputAmt) -> 
                    if cs == desiredCS
                    then 
                        -- Find corresponding output amount
                        let outputAmt = findAmount cs tn outputTokens
                            diffi = subtractInteger inputAmt outputAmt
                        in if diffi `greaterThanInteger` 0 
                        then addInteger acc diffi
                        else acc
                    else acc
                    ) 0 inputTokens
            in countMovedTokens

        -- Helper function to find amount of a specific token in a flattened value list
        findAmount :: Plutus.V2.Ledger.Api.CurrencySymbol -> Plutus.V2.Ledger.Api.TokenName -> [(Plutus.V2.Ledger.Api.CurrencySymbol, Plutus.V2.Ledger.Api.TokenName, Integer)] -> Integer
        findAmount cs tn tokens = 
            case filter (\(c, t, _) -> c == cs && t == tn) tokens of
                [] -> 0
                (_, _, amt):_ -> amt

        -- tokensRequested: Gets Value of tokens being spent from script
        -- Helper function for token movement tracking
        tokensRequested :: Plutus.V2.Ledger.Api.Value
        tokensRequested = getTxInValueOnly utxosSpentFromSc

        -- getTxInValueOnly: Extracts Value from transaction inputs
        -- Helper function for input validation
        getTxInValueOnly :: [Plutus.V2.Ledger.Api.TxInInfo] -> Plutus.V2.Ledger.Api.Value
        getTxInValueOnly []             = Plutus.V2.Ledger.Api.singleton Plutus.V2.Ledger.Api.adaSymbol Plutus.V2.Ledger.Api.adaToken 0
        getTxInValueOnly (i : is)       = Plutus.V2.Ledger.Api.txOutValue (Plutus.V2.Ledger.Api.txInInfoResolved i) <> (getTxInValueOnly is)

        -- getTxOutValueOnly: Extracts Value from transaction outputs
        -- Helper function for output validation
        getTxOutValueOnly :: [(Plutus.V2.Ledger.Api.OutputDatum, Plutus.V2.Ledger.Api.Value)] -> Plutus.V2.Ledger.Api.Value
        getTxOutValueOnly []             = Plutus.V2.Ledger.Api.singleton Plutus.V2.Ledger.Api.adaSymbol Plutus.V2.Ledger.Api.adaToken 0
        getTxOutValueOnly ((_, v) : tos) = v <> (getTxOutValueOnly tos)

        -- utxosSpentFromSc: Gets inputs originating from the script
        -- Identifies which UTXOs are being spent from the contract
        utxosSpentFromSc :: [Plutus.V2.Ledger.Api.TxInInfo]
        utxosSpentFromSc = filter (\u -> (Plutus.V2.Ledger.Api.txOutAddress (Plutus.V2.Ledger.Api.txInInfoResolved u)) PlutusTx.Prelude.== scriptHashAddress (ownHash ctx)) $ txInfoInputs info

        -- numDesiredTokensReceived: Calculates net movement of tokens into contract
        -- Validates that the correct number of NFTs are being deposited
        numDesiredTokensReceived :: Integer
        numDesiredTokensReceived = 
            let desiredCS = desiredPolicyID scriptParams
                scriptTxOuts = scriptOutputsAt (ownHash ctx) info
                outputValues = getTxOutValueOnly scriptTxOuts
                inputValues = getTxInValueOnly utxosSpentFromSc
                -- Count tokens that are in output but not in input (or more in output)
                outputTokens = flattenValue outputValues
                inputTokens = flattenValue inputValues
                countMovedTokens = foldl (\acc (cs, tn, outputAmt) ->
                    if cs == desiredCS
                    then
                        let inputAmt = findAmount cs tn inputTokens
                            diffi = subtractInteger outputAmt inputAmt
                        in if diffi `greaterThanInteger` 0
                        then addInteger acc diffi
                        else acc
                    else acc
                    ) 0 outputTokens
            in countMovedTokens
        
        -- numUnlistedTokensRequested: Checks for unauthorized token types
        -- Prevents withdrawal of tokens not matching the desired policy ID
        numUnlistedTokensRequested :: Integer
        numUnlistedTokensRequested = let valuedCSs = [(desiredPolicyID scriptParams), Plutus.V2.Ledger.Api.adaSymbol]
                                         producedCSs = symbols tokensRequested
                                     in  length (removeCurrencySymbolsFromList valuedCSs producedCSs)

        -- removeCurrencySymbolsFromList: Filters currency symbols
        -- Helper function for token policy validation
        removeCurrencySymbolsFromList :: [Plutus.V2.Ledger.Api.CurrencySymbol] -> [Plutus.V2.Ledger.Api.CurrencySymbol] -> [Plutus.V2.Ledger.Api.CurrencySymbol]
        removeCurrencySymbolsFromList [] cs = cs
        removeCurrencySymbolsFromList _  [] = []
        removeCurrencySymbolsFromList csToRemove (c : cs) = if c `elem` csToRemove
                                                            then removeCurrencySymbolsFromList csToRemove cs
                                                            else c : removeCurrencySymbolsFromList csToRemove cs

{- Contract Compilation and Deployment Functions -}

{-# INLINABLE mkWrappedNFTSwapValidator #-}
mkWrappedNFTSwapValidator :: Plutus.V2.Ledger.Api.BuiltinData -> Plutus.V2.Ledger.Api.BuiltinData -> Plutus.V2.Ledger.Api.BuiltinData -> Plutus.V2.Ledger.Api.BuiltinData -> ()
mkWrappedNFTSwapValidator scriptParams _ redeemer ctx =
    let unwrappedScriptParams = PlutusTx.unsafeFromBuiltinData scriptParams :: ContractParam
        unwrappedRedeemer = PlutusTx.unsafeFromBuiltinData redeemer :: RandomnessRedeemer
        unwrappedCtx = PlutusTx.unsafeFromBuiltinData ctx :: Plutus.V2.Ledger.Api.ScriptContext
    in if mkNFTSwapValidator unwrappedScriptParams () unwrappedRedeemer unwrappedCtx
       then ()
       else PlutusTx.Prelude.error ()

validatorWOParameters :: PlutusTx.CompiledCode (Plutus.V2.Ledger.Api.BuiltinData -> Plutus.V2.Ledger.Api.BuiltinData -> Plutus.V2.Ledger.Api.BuiltinData -> Plutus.V2.Ledger.Api.BuiltinData -> ())
validatorWOParameters = $$(PlutusTx.compile [|| mkWrappedNFTSwapValidator ||])

validatorWParameters :: ContractParam -> Plutus.V2.Ledger.Api.Validator
validatorWParameters scriptParams = Plutus.V2.Ledger.Api.mkValidatorScript $ validatorWOParameters `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData scriptParams)

{- Helper functions for Contract Deployment -}

-- saveValidatorWOParameters: Saves unparameterized validator to file
saveValidatorWOParameters :: IO()
saveValidatorWOParameters = writeCodeToFile "assets/random_swap_uninitialized.plutus" validatorWOParameters

-- saveValidatorWParameters: Saves parameterized validator to file
-- Creates unique filename based on contract owner
saveValidatorWParameters :: ContractParam -> IO ()
saveValidatorWParameters scriptParams = writeValidatorToFile (printf "assets/random-swap-%s.plutus" $ show (contractOwner scriptParams)) $ validatorWParameters scriptParams