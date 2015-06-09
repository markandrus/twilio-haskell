{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Conference.Participant
  ( -- * Resource
    Participant(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Monoid
import Data.Time.Clock
import GHC.Generics
import Network.URI

import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource
import Twilio.Types

data Participant = Participant
  { callSID                :: !CallSID
  , conferenceSID          :: !ConferenceSID
  , dateCreated            :: !UTCTime
  , dateUpdated            :: !UTCTime
  , accountSID             :: !AccountSID
  , muted                  :: !Bool
  , startConferenceOnEnter :: !Bool
  , endConferenceOnExit    :: !Bool
  , uri                    :: !URI
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance FromJSON Participant where
  parseJSON (Object v) = Participant
    <$>  v .: "call_sid"
    <*>  v .: "conference_sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "muted"
    <*>  v .: "start_conference_on_enter"
    <*>  v .: "end_conference_on_exit"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
  parseJSON _ = mzero

instance Get2 ConferenceSID CallSID Participant where
  get2 (getSID -> conferenceSID) (getSID -> callSID) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Conferences/" <> conferenceSID <> "/Participants/" <> callSID <> ".json")
