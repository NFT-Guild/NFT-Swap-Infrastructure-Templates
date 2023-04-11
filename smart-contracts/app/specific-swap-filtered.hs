{-# LANGUAGE OverloadedStrings          #-}

module Main
    ( main
    ) where

import Control.Exception    (throwIO)

import Data.String          (IsString (..))
import System.Environment   (getArgs)
import SpecificSwapFiltered (validator, ContractParam(..))
import Utils         (writeValidator, splitStringByDelim, bytesFromHex, stringToByteString)
import qualified Ledger
import Ledger.Value (tokenName)
-- utility library to be able to get object types
import Data.Typeable

main :: IO ()
main = do
    [file, pubkeyhash', policyid', tokenNamesCSV'] <- getArgs
    let pubkeyhash    = pubkeyhash'
        policyid      = policyid'
        tnStringCSV   = tokenNamesCSV'
        tnHexList     = splitStringByDelim ',' tnStringCSV
        tnBsList      = map stringToByteString tnHexList
        tnBsConvList  = map bytesFromHex tnBsList
        tnAllowedList = map tokenName tnBsConvList
        contParam     = ContractParam { contractOwner = Ledger.PaymentPubKeyHash $ fromString pubkeyhash
                                      , desiredPolicyID = fromString policyid
                                      , tokensAllowedToSwap = tnAllowedList }
        typeOfContractOwner = typeOf (contractOwner contParam)
        typeOfDesiredPolicy = typeOf (desiredPolicyID contParam)
        typeOfTnAllowedList = typeOf (tokensAllowedToSwap contParam)
    e <- writeValidator file $ validator $ contParam
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> mapM_ putStrLn [ "_______________________________________________"
                                   , " Contract saved to file        : " ++ file
                                   , " PubKeyHash of owner wallet    : " ++ pubkeyhash
                                   , " Desired Policy ID             : " ++ policyid 
                                   , " Parameter to contract         : " ++ show contParam
                                   , " contractOwner       (obj type): " ++ show typeOfContractOwner
                                   , " desiredPolicyID     (obj type): " ++ show typeOfDesiredPolicy
                                   , " tokensAllowedToSwap (obj type): " ++ show typeOfTnAllowedList
                                   , "_______________________________________________"]
