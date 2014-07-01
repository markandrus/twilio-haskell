module Main where

import Twilio.Types

import Twilio.Accounts          as Accounts
import Twilio.Applications      as Applications
import Twilio.Calls             as Calls
import Twilio.ConnectApps       as ConnectApps
import Twilio.Messages          as Messages
import Twilio.OutgoingCallerIDs as OutgoingCallerIDs
import Twilio.PhoneNumbers      as PhoneNumbers
import Twilio.UsageRecords      as UsageRecords

import Control.Monad (sequence_)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

-- | Print calls.
main :: IO ()
main = runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN")
     $ sequence_
       [ Accounts.get          >>= liftIO . print
       , Applications.get      >>= liftIO . print
       , Calls.get             >>= liftIO . print
       , ConnectApps.get       >>= liftIO . print
       , Messages.get          >>= liftIO . print
       , OutgoingCallerIDs.get >>= liftIO . print
       , PhoneNumbers.get      >>= liftIO . print
       , UsageRecords.get      >>= liftIO . print ]
