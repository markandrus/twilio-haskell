{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Conference
  ( -- * Resource
    Conference(..)
    -- * Types
  , ConferenceStatus(..)
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

data Conference = Conference
  { sid          :: !ConferenceSID
  , friendlyName :: !Text
  , status       :: !ConferenceStatus
  , dateCreated  :: !UTCTime
  , dateUpdated  :: !UTCTime
  , accountSID   :: !AccountSID
  , uri          :: !URI
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance FromJSON Conference where
  parseJSON (Object v) = Conference
    <$>  v .: "sid"
    <*>  v .: "friendly_name"
    <*>  v .: "status"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

{- Types -}

data ConferenceStatus
  = Init
  | InProgress
  | Completed
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON ConferenceStatus where
  parseJSON (String "init")        = return Init
  parseJSON (String "in-progress") = return InProgress
  parseJSON (String "completed")   = return Completed
  parseJSON _ = mzero

instance Get1 ConferenceSID Conference where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Conferences/" <> sid <> ".json")
