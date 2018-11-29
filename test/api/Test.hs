{-#OPTIONS_GHC -w #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Network.URI
import System.Environment
import Twilio
import Twilio.Account (Account)
import Twilio.Account               as Account
import Twilio.Accounts (Accounts)
import Twilio.Accounts              as Accounts
import Twilio.Addresses             as Addresses
import Twilio.APIKey (APIKey)
import Twilio.APIKey                as APIKey
import Twilio.APIKeys (APIKeys)
import Twilio.APIKeys               as APIKeys
import Twilio.Applications          as Applications
import Twilio.AuthorizedConnectApps as AuthorizedConnectApps
import Twilio.AvailablePhoneNumbers as AvailablePhoneNumbers
import Twilio.Calls (Calls)
import Twilio.Calls                 as Calls
import Twilio.Call (Call)
import Twilio.Call                  as Call
import Twilio.ConnectApps           as ConnectApps
import Twilio.IncomingPhoneNumbers  as IncomingPhoneNumbers
import Twilio.Messages (Messages)
import Twilio.Messages              as Messages
import Twilio.Message (Message)
import Twilio.Message               as Message
import Twilio.OutgoingCallerIDs     as OutgoingCallerIDs
import Twilio.Queues (Queues)
import Twilio.Queues                as Queues
import Twilio.Queue (Queue)
import Twilio.Queue                 as Queue
import Twilio.Recordings            as Recordings
import Twilio.ShortCode (ShortCode)
import Twilio.ShortCode             as ShortCode
import Twilio.ShortCodes (ShortCodes)
import Twilio.ShortCodes            as ShortCodes
import Twilio.Tokens (Token)
import Twilio.Tokens                as Tokens
import Twilio.Transcriptions        as Transcriptions
import Twilio.UsageRecords          as UsageRecords
import Twilio.UsageTriggers         as UsageTriggers

import Twilio.Internal.Resource (post)

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
    , Queues.get                   >>= liftIO . print
    , Recordings.get               >>= liftIO . print
    , Transcriptions.get           >>= liftIO . print
    , UsageRecords.get             >>= liftIO . print
    , UsageTriggers.get            >>= liftIO . print ]

  -- account { sid = accountSID } <- testPOSTAccounts
  testGETAccounts
  -- testGETAccount accountSID

  testGETAPIKeys

  Call { Call.sid = callSID } <- testPOSTCalls
  testGETCalls
  testGETCall callSID

  Message { Message.sid = messageSID } <- testPOSTMessages
  testGETMessages
  testGETMessage messageSID

  Queue { Queue.sid = queueSID } <- testPOSTQueues
  testGETQueues
  testGETQueue queueSID
  testDELETEQueue queueSID

  testGETShortCodes

  testPOSTTokens

  return ()

{- Accounts -}

testPOSTAccounts :: Twilio Account
testPOSTAccounts = do
  liftIO $ putStrLn "POST /Accounts"
  account <- Accounts.post Nothing
  liftIO $ print account
  return account

testGETAccounts :: Twilio Accounts
testGETAccounts = do
  liftIO $ putStrLn "GET /Accounts"
  accounts <- Accounts.get
  liftIO $ print accounts
  return accounts

{- Account -}

testGETAccount :: AccountSID -> Twilio Account
testGETAccount accountSID = do
  liftIO . putStrLn . unpack $ "GET /Accounts/" <> getSID accountSID
  account <- Account.get accountSID
  liftIO $ print account
  return account

{- Api Keys -}

testGETAPIKeys :: Twilio APIKeys
testGETAPIKeys = do
  liftIO $ putStrLn "GET /Keys"
  apiKeys <- APIKeys.get
  liftIO $ print apiKeys
  return apiKeys

{- Api Key -}

testGETAPIKey :: APIKeySID -> Twilio APIKey
testGETAPIKey apiKeySID = do
  liftIO . putStrLn . unpack $ "GET /Keys/" <> getSID apiKeySID
  apiKey <- APIKey.get apiKeySID
  liftIO $ print apiKey
  return apiKey

{- Calls -}

testPOSTCalls :: Twilio Call
testPOSTCalls = do
  liftIO $ putStrLn "POST /Calls"
  let Just url = parseAbsoluteURI "https://demo.twilio.com/welcome/voice/"
  testPhone <- liftIO $ getTestPhoneWithDefault "+14157671887"
  (call :: Call) <- Twilio.Internal.Resource.post
    (testPhone :: Text) (testPhone :: Text) url
  liftIO $ print call
  return call

testGETCalls :: Twilio Calls
testGETCalls = do
  liftIO $ putStrLn "GET /Calls"
  calls <- Calls.get
  liftIO $ print calls
  return calls

{- Call -}

testGETCall :: CallSID -> Twilio Call
testGETCall callSID = do
  liftIO . putStrLn . unpack $ "GET /Calls/" <> getSID callSID
  call <- Call.get callSID
  liftIO $ print call
  return call

{- Messages -}

testGETMessages :: Twilio Messages
testGETMessages = do
  liftIO $ putStrLn "GET /Messages"
  messages <- Messages.get
  liftIO $ print messages
  return messages

testPOSTMessages :: Twilio Message
testPOSTMessages = do
  liftIO $ putStrLn "POST /Messages"
  testPhone <- liftIO $ getTestPhoneWithDefault "+14157671887"
  let body = PostMessage "+12027621401" testPhone "Hello" Nothing
  message <- Messages.post body
  liftIO $ print message
  return message

{- Message -}

testGETMessage :: MessageSID -> Twilio Message
testGETMessage messageSID = do
  liftIO . putStrLn . unpack $ "GET /Message/" <> getSID messageSID
  message <- Message.get messageSID
  liftIO $ print message
  return message

{- Queues -}

testPOSTQueues :: Twilio Queue
testPOSTQueues = do
  liftIO $ putStrLn "POST /Queues"
  let body = PostQueues (Just "Some Queue") (Just 3)
  queue <- Queues.post body
  liftIO $ print queue
  return queue

testGETQueues :: Twilio Queues
testGETQueues = do
  liftIO $ putStrLn "GET /Queues"
  queues <- Queues.get
  liftIO $ print queues
  return queues

{- Queue -}

testGETQueue :: QueueSID -> Twilio Queue
testGETQueue queueSID = do
  liftIO . putStrLn . unpack $ "GET /Queues/" <> getSID queueSID
  queue <- Queue.get queueSID
  liftIO $ print queue
  return queue

testDELETEQueue :: QueueSID -> Twilio ()
testDELETEQueue queueSID = do
  liftIO . putStrLn . unpack $ "DELETE /Queues/" <> getSID queueSID
  Queue.delete queueSID

{- Short Codes -}

testGETShortCodes :: Twilio ShortCodes
testGETShortCodes = do
  liftIO $ putStrLn "GET /ShortCodes"
  queues <- ShortCodes.get
  liftIO $ print queues
  return queues

{- Short Code -}

testGETShortCode :: ShortCodeSID -> Twilio ShortCode
testGETShortCode shortCodeSID = do
  liftIO . putStrLn . unpack $ "GET /ShortCodes/" <> getSID shortCodeSID
  queue <- ShortCode.get shortCodeSID
  liftIO $ print queue
  return queue

{- Tokens -}

testPOSTTokens :: Twilio Token
testPOSTTokens = do
  liftIO $ putStrLn "POST /Tokens"
  token <- Tokens.post Nothing
  liftIO $ print token
  return token

getTestPhoneWithDefault defaultPhone = do
  maybeTestPhone <- lookupEnv "TEST_PHONE"
  return (pack $ fromMaybe defaultPhone maybeTestPhone)
