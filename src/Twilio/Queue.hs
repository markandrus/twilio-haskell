{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Queue
  ( -- * Resource
    Queue(..)
  , Twilio.Queue.get
  , Twilio.Queue.delete
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Data
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Network.URI

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
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
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*> (v .: "uri" <&> parseRelativeReference
                    >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 QueueSID Queue where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Queues/" <> sid <> ".json")

get :: MonadThrow m => QueueSID -> TwilioT m Queue
get = Resource.get

instance Delete1 QueueSID where
  delete1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioDELETERequest
    ("/Queues/" <> sid <> ".json")

delete :: MonadThrow m => QueueSID -> TwilioT m ()
delete = Resource.delete
