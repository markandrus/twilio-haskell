module Main where

import qualified Twilio.Account as Account
import qualified Twilio.Application as Application
import qualified Twilio.Client as Client
import qualified Twilio.ConnectApp as ConnectApp
import qualified Twilio.Call as Call
import qualified Twilio.Message as Message
import qualified Twilio.OutgoingCallerID as OutgoingCallerID
import qualified Twilio.PhoneNumber as PhoneNumber

import Data.Maybe (fromJust)
import System.Environment (getEnv)

client :: IO Client.Client
client = do
  accountSID <- getEnv "ACCOUNT_SID"
  authToken  <- getEnv "AUTH_TOKEN"
  return . fromJust $ Client.client accountSID authToken

main :: IO ()
main = do
  -- calls <- Call.calls =<< client
  -- print calls
  -- phoneNumbers <- PhoneNumber.phoneNumbers =<< client
  -- print phoneNumbers
  -- accounts <- Account.accounts =<< client
  -- print accounts
  -- applications <- Application.applications =<< client
  -- print applications
  -- outgoingCallerIDs <- OutgoingCallerID.outgoingCallerIDs =<< client
  -- print outgoingCallerIDs
  connectApps <- ConnectApp.connectApps =<< client
  print connectApps
  -- messages <- Message.messages =<< client
  -- print messages
