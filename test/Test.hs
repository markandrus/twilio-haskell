module Main where

import Twilio.Client as Client
import Twilio.Account
import Twilio.Call

import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)

client :: IO Client
client = do
  accountSID <- getEnv "ACCOUNT_SID"
  authToken  <- getEnv "AUTH_TOKEN"
  return . fromJust $ Client.client accountSID authToken

calls' :: IO ()
calls' = do
  request <- fmap calls Main.client
  manager <- newManager tlsManagerSettings
  withResponse request manager $ \response -> do
    let bodyReader = responseBody response
    bs <- brConsume bodyReader
    let json = (decode $ LBS.fromChunks bs) :: Maybe Object
    print json
    return ()

main = calls'
