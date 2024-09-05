{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveAnyClass        #-}

module RandomSwap where

import Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash, ScriptContext (scriptContextTxInfo),TokenName (unTokenName),
                                  Validator, mkValidatorScript, CurrencySymbol, Value, adaSymbol, adaToken, singleton, OutputDatum (..), BuiltinByteString, TxInInfo (..), TxOut (..), Datum )
import Plutus.V2.Ledger.Contexts (scriptOutputsAt, ownHash, txSignedBy, TxInInfo (txInInfoResolved), TxInfo (..))
import PlutusTx                  (applyCode, compile, liftCode, UnsafeFromData (unsafeFromBuiltinData), CompiledCode, ToData (toBuiltinData), unstableMakeIsData, fromBuiltinData, makeLift, makeIsDataIndexed)
import PlutusTx.Prelude          (Bool (True, False), ($), traceIfFalse, traceError, otherwise, Integer, length, elem, (+), filter, mapMaybe, Maybe (..), lengthOfByteString, indexByteString, appendByteString, sha2_256, sliceByteString, (&&), emptyByteString, consByteString, (==), null, head, error, (-), foldr)
import Prelude                   (IO, (<>), Show (show), toInteger)
import Utilities                 (writeCodeToFile, writeValidatorToFile)
import Plutus.V1.Ledger.Value    (flattenValue, symbols)
import Plutus.V1.Ledger.Address  (scriptHashAddress)
import Text.Printf               (printf)
import PlutusTx.Builtins         (equalsInteger, greaterThanInteger, modInteger, divideInteger, subtractInteger, lessThanEqualsInteger, lessThanInteger, equalsByteString, multiplyInteger, addInteger, greaterThanEqualsInteger)

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
    | action redeemer `equalsInteger` 3 =
        let maybeDatum = findDatum ctx
            maybeNFTName = getNFTNameFromInput
        in if isJustCustom maybeDatum && isJustCustom maybeNFTName
            then
                let datumValue = fromJustCustom maybeDatum
                    nftNameValue = fromJustCustom maybeNFTName
                in traceIfFalse "Hash validation failed" $ calculateAndVerifyHash
                        nftNameValue
                        datumValue
                        (randomString redeemer)
                        (randomIntegerHex redeemer)
                        (targetValue redeemer)
                        (slot redeemer)
                        (selectionValue redeemer)
            else traceError "Reference input or NFT name not found"
    | otherwise = traceError "UNSUPPORTED ACTION"

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        {-# INLINABLE bytesToHex #-}
        bytesToHex :: BuiltinByteString -> BuiltinByteString
        bytesToHex bs =
            let go n acc
                    | equalsInteger n 0 = acc
                    | otherwise =
                        let byte = indexByteString bs (subtractInteger n 1)
                            highNibble = divideInteger byte 16
                            lowNibble = modInteger byte 16
                            hexChars = appendByteString (integerToHex highNibble) (integerToHex lowNibble)
                        in go (subtractInteger n 1) (appendByteString hexChars acc)
            in go (lengthOfByteString bs) emptyByteString

        isNothing :: Maybe a -> Bool
        isNothing Nothing = True
        isNothing _ = False

        isJustCustom :: Maybe a -> Bool
        isJustCustom (Just _) = True
        isJustCustom Nothing = False

        fromJustCustom :: Maybe a -> a
        fromJustCustom (Just x) = x
        fromJustCustom Nothing = error ()

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

        getFirstTokenName :: Value -> Maybe TokenName
        getFirstTokenName value =
            let flattened = flattenValue value
            in if null flattened
                then Nothing
                else Just $ (\(_, tokenName, _) -> tokenName) (head flattened)

        getNFTNameFromInput :: Maybe TokenName
        getNFTNameFromInput = getFirstTokenName tokensRequested

        {-# INLINABLE calculateAndVerifyHash #-}
        calculateAndVerifyHash :: TokenName -> RandomnessOracleDatum -> BuiltinByteString -> BuiltinByteString -> Integer -> Integer -> Integer -> Bool
        calculateAndVerifyHash nftName dat rString rIntHex tValue slt sValue =
            let (_, datumBlockHash, _) = blockData dat
                paddedSlot = padToLength 8 (integerToHex slt)
                combinedHex = appendByteString paddedSlot datumBlockHash
                combinedBytes = hexStringToBuiltinByteString combinedHex
                calculatedHash = sha2_256 combinedBytes
                randomInteger = byteStringToInteger (sliceByteString 0 8 calculatedHash)
                calculatedTargetValue = (randomInteger `divideInteger` 65536) `modInteger` 65536
                calculatedSelectedValue = calculateSelectedValue (unTokenName nftName) randomInteger
            in  calculatedHash `equalsByteString` rString &&
                sliceByteString 0 8 calculatedHash `equalsByteString` rIntHex &&
                tValue `equalsInteger` calculatedTargetValue &&
                sValue `equalsInteger` calculatedSelectedValue

        {-# INLINABLE calculateSelectedValue #-}
        calculateSelectedValue :: BuiltinByteString -> Integer -> Integer
        calculateSelectedValue nftName randomInteger =
            let nameLength = lengthOfByteString nftName
                position1 = subtractInteger (subtractInteger nameLength 1) (modInteger randomInteger 8)
                position2 = subtractInteger (subtractInteger nameLength 1) (modInteger (divideInteger randomInteger 256) 8)
                byte1 = indexByteString nftName position1
                byte2 = indexByteString nftName position2
            in addInteger (multiplyInteger byte1 256) byte2

        {-# INLINABLE integerToHex #-}
        integerToHex :: Integer -> BuiltinByteString
        integerToHex n = 
            let go m acc
                    | m == 0 = acc
                    | otherwise = go (m `divideInteger` 256) (consByteString (m `modInteger` 256) acc)
            in go n emptyByteString

        {-# INLINABLE intToHexChar #-}
        intToHexChar :: Integer -> Integer
        intToHexChar n
            | lessThanInteger n 10 = addInteger n 48  -- '0' to '9'
            | otherwise = addInteger n 87  -- 'a' to 'f'

        byteStringToInteger :: BuiltinByteString -> Integer
        byteStringToInteger bs =
            let go n acc
                    | equalsInteger n 0 = acc
                    | otherwise = go (subtractInteger n 1) (addInteger (multiplyInteger acc 256) (indexByteString bs (subtractInteger n 1)))
            in go (lengthOfByteString bs) 0

        {-# INLINABLE padToLength #-}
        padToLength :: Integer -> BuiltinByteString -> BuiltinByteString
        padToLength len bs =
            let currentLength = lengthOfByteString bs
                padLength = subtractInteger len currentLength
            in if lessThanEqualsInteger padLength 0
            then bs
            else appendByteString (repeatByteString padLength 48) bs  -- '0' is 48 in ASCII

        {-# INLINABLE hexStringToBuiltinByteString #-}
        hexStringToBuiltinByteString :: BuiltinByteString -> BuiltinByteString
        hexStringToBuiltinByteString hexStr =
            let len = lengthOfByteString hexStr
                isOdd = modInteger len 2 == 1
                paddedHexStr = if isOdd then appendByteString hexStr "0" else hexStr
                go n acc
                    | equalsInteger n 0 = acc
                    | otherwise = 
                        let index1 = subtractInteger (multiplyInteger n 2) 2
                            index2 = subtractInteger (multiplyInteger n 2) 1
                            char1 = indexByteString paddedHexStr index1
                            char2 = indexByteString paddedHexStr index2
                            byte = hexPairToInteger char1 char2
                            newAcc = consByteString byte acc
                        in go (subtractInteger n 1) newAcc
            in go (divideInteger (lengthOfByteString paddedHexStr) 2) emptyByteString

        {-# INLINABLE andBool #-}
        andBool :: Bool -> BuiltinByteString -> BuiltinByteString
        andBool b bs = if b then bs else bs

        {-# INLINABLE hexPairToInteger #-}
        hexPairToInteger :: Integer -> Integer -> Integer
        hexPairToInteger a b = 
            let valueA = hexCharToInteger a
                valueB = hexCharToInteger b
            in addInteger (multiplyInteger valueA 16) valueB

        {-# INLINABLE hexCharToInteger #-}
        hexCharToInteger :: Integer -> Integer
        hexCharToInteger c
            | greaterThanEqualsInteger c 48 && lessThanEqualsInteger c 57  = subtractInteger c 48       -- '0' to '9'
            | greaterThanEqualsInteger c 97 && lessThanEqualsInteger c 102 = addInteger (subtractInteger c 97) 10  -- 'a' to 'f'
            | greaterThanEqualsInteger c 65 && lessThanEqualsInteger c 70  = addInteger (subtractInteger c 65) 10  -- 'A' to 'F'
            | otherwise           = 0            -- Default to 0 for invalid characters

        {-# INLINABLE repeatByteString #-}
        repeatByteString :: Integer -> Integer -> BuiltinByteString
        repeatByteString n byte
            | lessThanEqualsInteger n 0 = emptyByteString
            | otherwise = consByteString byte (repeatByteString (subtractInteger n 1) byte)

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
