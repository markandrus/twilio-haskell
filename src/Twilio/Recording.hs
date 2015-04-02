{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Recording
  ( -- * Resource
    Recording(..)
  , Twilio.Recording.get
  ) where

import Control.Applicative
import Control.Error.Safe
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import Data.Time.Clock
import Network.URI

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}
data Recording = Recording
  { sid                  :: !RecordingSID
  , dateCreated          :: !UTCTime
  , dateUpdated          :: !UTCTime
  , accountSID           :: !AccountSID
  , callSID              :: !CallSID
  , duration             :: !(Maybe Int)
  , apiVersion           :: !APIVersion
  , uri                  :: !URI
  } deriving (Show, Eq)


instance FromJSON Recording where
  parseJSON (Object v) = Recording
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "call_sid"
    <*> (v .: "duration"     <&> fmap readZ
                             >>= maybeReturn')
    <*>  v .: "api_version"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 RecordingSID Recording where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Recordings/" <> sid <> ".json")

-- | Get a 'Recording' by 'RecordingSID'.
get :: MonadThrow m => RecordingSID -> TwilioT m Recording
get = Resource.get
