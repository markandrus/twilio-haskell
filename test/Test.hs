{-#LANGUAGE NamedFieldPuns #-}
module Main where

import Twilio.Types

import Twilio.Account               as Account
import Twilio.Accounts              as Accounts
import Twilio.Applications          as Applications
import Twilio.AuthorizedConnectApps as AuthorizedConnectApps
import Twilio.AvailablePhoneNumbers as AvailablePhoneNumbers
import Twilio.Calls                 as Calls
import Twilio.ConnectApps           as ConnectApps
import Twilio.IncomingPhoneNumbers  as IncomingPhoneNumbers
import Twilio.Messages              as Messages
import Twilio.OutgoingCallerIDs     as OutgoingCallerIDs
import Twilio.Recordings            as Recordings
import Twilio.Transcriptions        as Transcriptions
import Twilio.UsageRecords          as UsageRecords

import Control.Monad (forM_)
-- import Control.Monad (sequence_)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

main :: IO ()
main = runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN") $ sequence_

  [ Accounts.get                 >>= liftIO . print
  , Applications.get             >>= liftIO . print
  , AuthorizedConnectApps.get    >>= liftIO . print
  , AvailablePhoneNumbers.get US >>= liftIO . print
  , Calls.get                    >>= liftIO . print
  , ConnectApps.get              >>= liftIO . print
  , IncomingPhoneNumbers.get     >>= liftIO . print
  , Messages.get                 >>= liftIO . print
  , OutgoingCallerIDs.get        >>= liftIO . print
  , UsageRecords.get             >>= liftIO . print
  , Transcriptions.get           >>= liftIO . print
  , Recordings.get               >>= liftIO . print ]

niam = runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN") $ do
  subAccounts <- fmap getList Accounts.get
  forM_ subAccounts $ \Account {Account.sid=subAccountSID} ->
    forAccount subAccountSID Calls.get >>= liftIO . print
