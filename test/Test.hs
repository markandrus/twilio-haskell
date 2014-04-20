module Main where

import qualified Twilio.Client as Client
import qualified Twilio.Call as Call

import Data.Maybe (fromJust)
import System.Environment (getEnv)

client :: IO Client.Client
client = do
  accountSID <- getEnv "ACCOUNT_SID"
  authToken  <- getEnv "AUTH_TOKEN"
  return . fromJust $ Client.client accountSID authToken

main :: IO ()
main = do
  calls <- Call.calls =<< client
  print calls
