{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Queue.Member
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Queue.Member
  ( -- * Resource
    Member(..)
  ) where

import Control.Monad
import Data.Aeson
import Data.Data
import Data.Time.Clock
import GHC.Generics
import Network.URI

import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource
import Twilio.Types

data Member = Member
  { callSID      :: !CallSID
  , dateEnqueued :: !UTCTime
  , waitTime     :: !Int
  , position     :: !Int
  , uri          :: !URI
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance FromJSON Member where
  parseJSON (Object v) = Member
    <$>  v .: "call_sid"
    <*> (v .: "date_enqueud" >>= parseDateTime)
    <*>  v .: "wait_time"
    <*>  v .: "position"
    <*> (v .: "uri" <&> parseRelativeReference
                    >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 QueueSID Member where
  get1 (getSID -> queueSID) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Queues/" <> queueSID <> "/Front.json")

instance Get2 QueueSID CallSID Member where
  get2 (getSID -> queueSID) (getSID -> callSID) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Queues/" <> queueSID <> "/Members/" <> callSID <> ".json")
