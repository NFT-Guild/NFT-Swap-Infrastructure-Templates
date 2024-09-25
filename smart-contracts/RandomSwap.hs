{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveAnyClass        #-}

module RandomSwap where

import Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash (..), ScriptContext (scriptContextTxInfo),TokenName (unTokenName, TokenName),
                                  Validator, mkValidatorScript, CurrencySymbol, Value, adaSymbol, adaToken, singleton, OutputDatum (..), BuiltinByteString, TxInInfo (..), TxOut (..), Datum, unionWith, Credential (..), ValidatorHash)
import Plutus.V2.Ledger.Contexts (scriptOutputsAt, txSignedBy, TxInInfo (txInInfoResolved), TxInfo (..), ownHash)
import PlutusTx                  (applyCode, compile, liftCode, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData), unstableMakeIsData, fromBuiltinData, makeLift, makeIsDataIndexed)
import PlutusTx.Prelude          (Bool (True, False), ($), traceIfFalse, traceError, otherwise, Integer, length, elem, (+), filter, mapMaybe, Maybe (..), lengthOfByteString, indexByteString, appendByteString, sliceByteString, (&&), emptyByteString, consByteString, (==), null, head, error, (/=), Monoid, foldr, mempty, mappend, (.), zero, foldl, any, (-), (*))
import Prelude                   (IO, (<>), Show (show), not)
import Utilities                 (writeCodeToFile, writeValidatorToFile)
import Plutus.V1.Ledger.Value    (flattenValue, symbols, valueOf)
import Plutus.V1.Ledger.Address  (scriptHashAddress, Address(..))
import Text.Printf               (printf)
import PlutusTx.Builtins         (equalsInteger, greaterThanInteger, modInteger, divideInteger, subtractInteger, lessThanEqualsInteger, equalsByteString, multiplyInteger, addInteger, sha2_256, greaterThanEqualsInteger)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Contract parameter object to save state about the contract owner and desired policy
data ContractParam = ContractParam
    { contractOwner   :: !PubKeyHash
    , desiredPolicyID :: !CurrencySymbol
    , authorizedTxSigner :: !PubKeyHash
    }
PlutusTx.makeLift ''ContractParam
PlutusTx.unstableMakeIsData ''ContractParam

data RandomnessRedeemer = RandomnessRedeemer
    { randomString :: !BuiltinByteString
    , randomIntegerHex :: !BuiltinByteString
    , targetValue :: !Integer
    , slot :: !Integer
    , pos1 :: !Integer
    , pos2 :: !Integer
    , selectionValue :: !Integer
    , diff :: !Integer
    , action :: !Integer
    }
PlutusTx.makeLift ''RandomnessRedeemer
PlutusTx.unstableMakeIsData ''RandomnessRedeemer

data RandomnessOracleDatum = RandomnessOracleDatum
    { field1 :: BuiltinData
    , field2 :: BuiltinData
    , blockData ::  (Integer, BuiltinByteString, Integer)
    , field4 :: BuiltinData
    , field5 :: BuiltinData
    , field6 :: BuiltinData
    }
PlutusTx.makeLift ''RandomnessOracleDatum
PlutusTx.makeIsDataIndexed ''RandomnessOracleDatum [('RandomnessOracleDatum, 0)]

{-# INLINABLE mkNFTSwapValidator #-}
--                    Parameter to script      Datum     Redeemer                ScriptContext    Result
mkNFTSwapValidator :: ContractParam         -> ()     -> RandomnessRedeemer   -> ScriptContext -> Bool
mkNFTSwapValidator scriptParams _ redeemer ctx 
    | action redeemer `equalsInteger` 0   =
            let numRequested              = numDesiredTokensRequested
                numReceived               = numDesiredTokensReceived
                receivedOracle            = traceIfFalse "Did not receive oracle" hasReferenceInputs
                requestedAndReceivedEqual = traceIfFalse "Number of tokens requested not equal to number of tokens received" $ numRequested `equalsInteger` numReceived
                atLeastOneTokenSwapped    = traceIfFalse "At least one token is required for swap operation to be performed" $ numReceived `greaterThanInteger` 0
                noUnlistedTokensWithdrawn = traceIfFalse "Unlisted tokens can only be withdrawn by the contract owner"       $ numUnlistedTokensRequested `equalsInteger` 0
            in                              traceIfFalse "SWAP FAILED"                                                       $ requestedAndReceivedEqual &&
                                                                                                                                atLeastOneTokenSwapped &&
                                                                                                                                noUnlistedTokensWithdrawn &&
                                                                                                                                receivedOracle
    | action redeemer `equalsInteger` 1 =
            traceIfFalse "CLEANUP FAILED: Operation can only be performed by contract owner " performedByContractOwner
    | action redeemer == 10 =
        let maybeNFTName = getNFTNameFromInput ctx
            nftName = if isNothing maybeNFTName then error () else fromJust maybeNFTName
            rString = randomString redeemer
            rIntHex = randomIntegerHex redeemer
            tValue = targetValue redeemer
            sValue = selectionValue redeemer
            datumValue = fromJust (findDatum ctx)
            (_, datumBlockHash, _) = blockData datumValue
            calculatedHash = validateAndHashSlot (slot redeemer) datumBlockHash
        in traceIfFalse "Hash validation failed" (equalsByteString calculatedHash rString) &&
        traceIfFalse "Target value mismatch" (equalsInteger (calculateTargetValue rIntHex) tValue) &&
        traceIfFalse "Selection value mismatch" (equalsInteger (calculateSelectedValue nftName rIntHex) sValue)
    | otherwise = traceError "UNSUPPORTED ACTION"

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        validateAndHashSlot :: Integer -> BuiltinByteString -> BuiltinByteString
        validateAndHashSlot slt blockHash = 
            let slotHex = integerToHex slt
                paddedSlot = padToLength 4 slotHex
            in sha2_256 (appendByteString paddedSlot blockHash)

        hexToInt :: BuiltinByteString -> Integer
        hexToInt bs =
            let hexValue c
                    | c `greaterThanEqualsInteger` 48 && c `lessThanEqualsInteger` 57 = subtractInteger c 48  -- '0' to '9'
                    | c `greaterThanEqualsInteger` 65 && c `lessThanEqualsInteger` 70 = subtractInteger c 55  -- 'A' to 'F'
                    | c `greaterThanEqualsInteger` 97 && c `lessThanEqualsInteger` 102 = subtractInteger c 87 -- 'a' to 'f'
                    | otherwise = 0  -- Invalid hex character
                go n acc
                    | n == lengthOfByteString bs = acc
                    | otherwise =
                        let hexDigit = hexValue (indexByteString bs n)
                        in go (addInteger n 1) (addInteger (multiplyInteger acc 16) hexDigit)
            in go 0 0

        -- Helper function to convert 4 bytes to Integer
        fourBytesToInteger :: BuiltinByteString -> Integer
        fourBytesToInteger bs =
            let b0 = indexByteString bs 0
                b1 = indexByteString bs 1
                b2 = indexByteString bs 2
                b3 = indexByteString bs 3
            in b0 * 16777216 + b1 * 65536 + b2 * 256 + b3

        padToLength :: Integer -> BuiltinByteString -> BuiltinByteString
        padToLength len bs =
            let currentLength = lengthOfByteString bs
                padLength = subtractInteger len currentLength
            in if lessThanEqualsInteger padLength 0
            then bs
            else appendByteString (repeatByteString padLength 0) bs

        repeatByteString :: Integer -> Integer -> BuiltinByteString
        repeatByteString n byte
            | lessThanEqualsInteger n 0 = emptyByteString
            | otherwise = consByteString byte (repeatByteString (subtractInteger n 1) byte)

        calculateTargetValue :: BuiltinByteString -> Integer
        calculateTargetValue rIntHex =
            let randomInteger = fourBytesToInteger rIntHex
            in modInteger randomInteger 65536

        bytesToHex :: BuiltinByteString -> BuiltinByteString
        bytesToHex bs =
            let hexChars = "0123456789ABCDEF"
                go n acc
                    | n == lengthOfByteString bs = acc
                    | otherwise =
                        let byte = indexByteString bs n
                            hi = divideInteger byte 16
                            lo = modInteger byte 16
                            hexPair = appendByteString 
                                (sliceByteString hi 1 hexChars) 
                                (sliceByteString lo 1 hexChars)
                        in go (addInteger n 1) (appendByteString acc hexPair)
            in go 0 emptyByteString

        
        getSpentTokens :: ScriptContext -> Value
        getSpentTokens context =
            let ownScriptHash = ownHash context
                txInfo = scriptContextTxInfo context
                isOwnInput txIn = case txOutAddress (txInInfoResolved txIn) of
                                    Address (ScriptCredential vh) _ -> vh == ownScriptHash
                                    _ -> False
                ownInputs = filter isOwnInput (txInfoInputs txInfo)
                inputValue = foldl (\acc txIn -> unionWith (+) acc (txOutValue (txInInfoResolved txIn))) zero ownInputs
                outputValue = foldl (\acc txOut -> unionWith (+) acc (txOutValue txOut)) zero (filter (\txOut -> txOutAddress txOut == Address (ScriptCredential ownScriptHash) Nothing) (txInfoOutputs txInfo))
                spentValue = subtractValue inputValue outputValue
            in filterValue (\cs tn -> not (isValueSentToContract cs tn txInfo ownScriptHash)) spentValue

        getNFTNameFromInput :: ScriptContext -> Maybe TokenName
        getNFTNameFromInput context =
            let spentTokens = getSpentTokens context
                nonAdaTokens = filterValue (\cs _ -> cs /= adaSymbol) spentTokens
            in case flattenValue nonAdaTokens of
                [] -> Nothing
                ((cs, tn, amt):_) -> if amt `greaterThanInteger` 0 && isNFTFromContract cs tn context then Just tn else Nothing

        isNFTFromContract :: CurrencySymbol -> TokenName -> ScriptContext -> Bool
        isNFTFromContract cs tn context =
            let ownScriptHash = ownHash context
                txInfo = scriptContextTxInfo context
                isOwnInput txIn = case txOutAddress (txInInfoResolved txIn) of
                                    Address (ScriptCredential vh) _ -> vh == ownScriptHash
                                    _ -> False
                ownInputs = filter isOwnInput (txInfoInputs txInfo)
            in any (\txIn -> valueOf (txOutValue (txInInfoResolved txIn)) cs tn `greaterThanInteger` 0) ownInputs

        isValueSentToContract :: CurrencySymbol -> TokenName -> TxInfo -> ValidatorHash -> Bool
        isValueSentToContract cs tn txInfo ownScriptHash =
            let nonScriptInputs = filter (\txIn -> case txOutAddress (txInInfoResolved txIn) of
                                                    Address (ScriptCredential vh) _ -> vh /= ownScriptHash
                                                    _ -> True) (txInfoInputs txInfo)
            in any (\txIn -> valueOf (txOutValue (txInInfoResolved txIn)) cs tn `greaterThanInteger` 0) nonScriptInputs

        -- Helper function to subtract Values
        subtractValue :: Value -> Value -> Value
        subtractValue v1 v2 = unionWith (-) v1 v2

        -- Helper function to filter Value
        filterValue :: (CurrencySymbol -> TokenName -> Bool) -> Value -> Value
        filterValue f v = 
            foldMap (\(cs, tn, amt) -> if f cs tn then singleton cs tn amt else zero) (flattenValue v)

        -- Helper function for foldMap if not available
        foldMap :: (Monoid m) => (a -> m) -> [a] -> m
        foldMap f = foldr (mappend . f) mempty

        isNothing :: Maybe a -> Bool
        isNothing Nothing = True
        isNothing _ = False

        fromJust :: Maybe a -> a
        fromJust (Just x) = x
        fromJust Nothing = traceError "fromJust: Nothing"
    
        findDatum :: ScriptContext -> Maybe RandomnessOracleDatum
        findDatum context =
            let referenceInputs = txInfoReferenceInputs $ scriptContextTxInfo context
                datums = mapMaybe getTxOutDatum referenceInputs
            in if null datums
                then Nothing
                else PlutusTx.fromBuiltinData (PlutusTx.toBuiltinData (head datums))
         where
            getTxOutDatum :: TxInInfo -> Maybe Datum
            getTxOutDatum (TxInInfo _ txOut) =
                if isOutputDatum (txOutDatum txOut)
                then Just (getOutputDatum (txOutDatum txOut))
                else Nothing

            isOutputDatum :: OutputDatum -> Bool
            isOutputDatum (OutputDatum _) = True
            isOutputDatum _ = False

            getOutputDatum :: OutputDatum -> Datum
            getOutputDatum (OutputDatum d) = d
            getOutputDatum _ = error ()

        calculateSelectedValue :: TokenName -> BuiltinByteString -> Integer
        calculateSelectedValue nftName rIntHex =
            let hexName = bytesToHex (unTokenName nftName)
                hexLength = lengthOfByteString hexName
                randomInteger = fourBytesToInteger (sliceByteString 0 4 rIntHex)
                position1 = subtractInteger (subtractInteger hexLength 1) (modInteger randomInteger 8)
                position2 = subtractInteger (subtractInteger hexLength 1) (modInteger (divideInteger randomInteger 256) 8)
                byte1 = hexToInt (sliceByteString position1 2 hexName)
                byte2 = hexToInt (sliceByteString position2 2 hexName)
            in addInteger (multiplyInteger byte1 256) byte2

        {-# INLINABLE integerToHex #-}
        integerToHex :: Integer -> BuiltinByteString
        integerToHex n =
            let go m acc
                    | equalsInteger m 0 = acc
                    | otherwise = go (divideInteger m 256) (consByteString (modInteger m 256) acc)
            in go n emptyByteString

        -- check if the transaction has reference inputs
        hasReferenceInputs :: Bool
        hasReferenceInputs = length (txInfoReferenceInputs info) `greaterThanInteger` 0

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
        utxosSpentFromSc = filter (\u -> (txOutAddress (txInInfoResolved u)) PlutusTx.Prelude.== scriptHashAddress (ownHash ctx)) $ txInfoInputs info

        -- count number of tokens in flatten value that have a specific CurrencySymbol
        numOfTokensFromPolicy :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> Integer
        numOfTokensFromPolicy [] _ = 0
        numOfTokensFromPolicy ((vc, _, vi) : vs) cs = if vc PlutusTx.Prelude.== cs
                                                    then vi PlutusTx.Prelude.+ (numOfTokensFromPolicy vs cs)
                                                    else 0  PlutusTx.Prelude.+ (numOfTokensFromPolicy vs cs)

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


{-# INLINABLE mkWrappedNFTSwapValidator #-}
mkWrappedNFTSwapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTSwapValidator scriptParams _ redeemer ctx =
    let unwrappedScriptParams = PlutusTx.unsafeFromBuiltinData scriptParams :: ContractParam
        unwrappedRedeemer = PlutusTx.unsafeFromBuiltinData redeemer :: RandomnessRedeemer
        unwrappedCtx = PlutusTx.unsafeFromBuiltinData ctx :: ScriptContext
    in if mkNFTSwapValidator unwrappedScriptParams () unwrappedRedeemer unwrappedCtx
       then ()
       else PlutusTx.Prelude.error ()

validatorWOParameters :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorWOParameters = $$(PlutusTx.compile [|| mkWrappedNFTSwapValidator ||])

validatorWParameters :: ContractParam -> Validator
validatorWParameters scriptParams = mkValidatorScript $ validatorWOParameters `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData scriptParams)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveValidatorWOParameters :: IO()
saveValidatorWOParameters = writeCodeToFile "assets/random_swap_uninitialized.plutus" validatorWOParameters

saveValidatorWParameters :: ContractParam -> IO ()
saveValidatorWParameters scriptParams = writeValidatorToFile (printf "assets/random-swap-%s.plutus" $ show (contractOwner scriptParams)) $ validatorWParameters scriptParams