{-# LANGUAGE OverloadedStrings          #-}

module Main
    ( main
    ) where

import Control.Exception    (throwIO)

import Data.String          (IsString (..))
import System.Environment   (getArgs)
import SpecificSwap (validator, ContractParam(..))
import Utils         (writeValidator)
import qualified Ledger
-- utility library to be able to get object types
import Data.Typeable

main :: IO ()
main = do
    [file, pubkeyhash', policyid'] <- getArgs
    let pubkeyhash    = pubkeyhash'
        policyid      = policyid'
        contParam = ContractParam { contractOwner = Ledger.PaymentPubKeyHash $ fromString pubkeyhash
                                  , desiredPolicyID = fromString policyid }
        typeOfContractOwner = typeOf (contractOwner contParam)
        typeOfDesiredPolicy = typeOf (desiredPolicyID contParam)
    e <- writeValidator file $ validator $ contParam
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> mapM_ putStrLn [ "_______________________________________________"
                                   , " Contract saved to file     : " ++ file
                                   , " PubKeyHash of owner wallet : " ++ pubkeyhash
                                   , " Desired Policy ID          : " ++ policyid 
                                   , " Parameter to contract      : " ++ show contParam
                                   , " contractOwner    (obj type): " ++ show typeOfContractOwner
                                   , " desiredPolicyID  (obj type): " ++ show typeOfDesiredPolicy
                                   , "_______________________________________________"]
