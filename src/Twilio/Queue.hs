{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Queue
  ( -- * Resource
    Queue(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Network.URI

import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource
import Twilio.Types

{- Resource -}

data Queue = Queue
  { sid             :: !QueueSID
  , friendlyName    :: !Text
  , currentSize     :: !Int
  , maxSize         :: !Int
  , averageWaitTime :: !Float
  , dateCreated     :: !UTCTime
  , dateUpdated     :: !UTCTime
  , uri             :: !URI
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance FromJSON Queue where
  parseJSON (Object v) = Queue
    <$>  v .: "sid"
    <*>  v .: "friendly_name"
    <*>  v .: "current_size"
    <*>  v .: "max_size"
    <*>  v .: "average_wait_time"
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
    <*> (v .: "uri" <&> parseRelativeReference
                    >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 QueueSID Queue where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Queues/" <> sid <> ".json")
