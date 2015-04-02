{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.OutgoingCallerID
  ( -- * Resource
    OutgoingCallerID(..)
  , PhoneNumberSID
  , Twilio.OutgoingCallerID.get
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Network.URI

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data OutgoingCallerID = OutgoingCallerID
  { sid          :: !PhoneNumberSID
  , dateCreated  :: !UTCTime
  , dateUpdated  :: !UTCTime
  , friendlyName :: !Text
  , accountSID   :: !AccountSID
  , phoneNumber  :: !Text
  , uri          :: !URI
  } deriving (Show, Eq)

instance FromJSON OutgoingCallerID where
  parseJSON (Object v) = OutgoingCallerID
    <$>  v .: "sid"
    <*> (v .: "date_created"  >>= parseDateTime)
    <*> (v .: "date_updated"  >>= parseDateTime)
    <*>  v .: "friendly_name"
    <*>  v .: "account_sid"
    <*>  v .: "phone_number"
    <*> (v .: "uri"           <&> parseRelativeReference
                              >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 PhoneNumberSID OutgoingCallerID where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/OutgoingCallerIds/" <> sid <> ".json")

-- | Get an 'OutgoingCallerID' by 'PhoneNumberSID'
get :: MonadThrow m => PhoneNumberSID -> TwilioT m OutgoingCallerID
get = Resource.get
