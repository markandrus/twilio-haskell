module Main where

import Control.Monad.IO.Class
import System.Environment
import Twilio
import Twilio.Account               as Account
import Twilio.Accounts              as Accounts
import Twilio.Addresses             as Addresses
import Twilio.Applications          as Applications
import Twilio.AuthorizedConnectApps as AuthorizedConnectApps
import Twilio.AvailablePhoneNumbers as AvailablePhoneNumbers
import Twilio.Calls                 as Calls
import Twilio.ConnectApps           as ConnectApps
import Twilio.IncomingPhoneNumbers  as IncomingPhoneNumbers
import Twilio.Messages              as Messages
import Twilio.OutgoingCallerIDs     as OutgoingCallerIDs
import Twilio.Recordings            as Recordings
import Twilio.Tokens                as Tokens
import Twilio.Transcriptions        as Transcriptions
import Twilio.UsageRecords          as UsageRecords
import Twilio.UsageTriggers         as UsageTriggers

main :: IO ()
main = runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN") $ do
  -- Test GET
  sequence_
    [ Accounts.get                 >>= liftIO . print
    , Addresses.get                >>= liftIO . print
    , Applications.get             >>= liftIO . print
    , AuthorizedConnectApps.get    >>= liftIO . print
    , AvailablePhoneNumbers.get US >>= liftIO . print
    , Calls.get                    >>= liftIO . print
    , ConnectApps.get              >>= liftIO . print
    , IncomingPhoneNumbers.get     >>= liftIO . print
    , Messages.get                 >>= liftIO . print
    , OutgoingCallerIDs.get        >>= liftIO . print
    , Recordings.get               >>= liftIO . print
    , Transcriptions.get           >>= liftIO . print
    , UsageRecords.get             >>= liftIO . print
    , UsageTriggers.get            >>= liftIO . print ]

  -- Test POST /Messages
  let body = PostMessage "+14158059869" "+14158059869" "Hello"
  message <- post body
  liftIO $ print message

  -- Test POST /Tokens
  token <- Tokens.post Nothing
  liftIO $ print token
