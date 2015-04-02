{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.IncomingPhoneNumber
  ( -- * Resource
    IncomingPhoneNumber(..)
  , Twilio.IncomingPhoneNumber.get
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

data IncomingPhoneNumber = IncomingPhoneNumber
  { sid                  :: !PhoneNumberSID
  , dateCreated          :: !UTCTime
  , dateUpdated          :: !UTCTime
  , friendlyName         :: !Text
  , accountSID           :: !AccountSID
  , phoneNumber          :: !Text
  , apiVersion           :: !APIVersion
  , voiceCallerIDLookup  :: !Bool
  , voiceURL             :: !(Maybe Text)
  , voiceMethod          :: !Text
  , voiceFallbackURL     :: !(Maybe Text)
  , voiceFallbackMethod  :: !Text
  , statusCallback       :: !(Maybe Text)
  , statusCallbackMethod :: !Text
  , voiceApplicationSID  :: !(Maybe ApplicationSID)
  , smsURL               :: !(Maybe Text)
  , smsMethod            :: !Text
  , smsFallbackURL       :: !(Maybe Text)
  , smsFallbackMethod    :: !Text
  , smsApplicationSID    :: !(Maybe ApplicationSID)
  , capabilities         :: !Capabilities
  , addressRequirements  :: !AddressRequirement
  , uri                  :: !URI
  } deriving (Eq, Show)

instance FromJSON IncomingPhoneNumber where
  parseJSON (Object v) = IncomingPhoneNumber
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "friendly_name"
    <*>  v .: "account_sid"
    <*>  v .: "phone_number"
    <*>  v .: "api_version"
    <*>  v .: "voice_caller_id_lookup"
    <*> (v .: "voice_url" <&> getNonEmptyText)
    <*>  v .: "voice_method"
    <*> (v .: "voice_fallback_url" <&> (join . fmap getNonEmptyText))
    <*>  v .: "voice_fallback_method"
    <*> (v .: "status_callback"  <&> getNonEmptyText)
    <*>  v .: "status_callback_method"
    <*> (v .: "voice_application_sid" <&> (join . fmap parseSID . getNonEmptyText))
    <*> (v .: "sms_url"          <&> getNonEmptyText)
    <*>  v .: "sms_method"
    <*> (v .: "sms_fallback_url" <&> getNonEmptyText)
    <*>  v .: "sms_fallback_method"
    <*> (v .: "sms_application_sid" <&> (join . fmap parseSID . getNonEmptyText))
    <*> (v .: "capabilities" >>= parseJSON)
    <*>  v .: "address_requirements"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 PhoneNumberSID IncomingPhoneNumber where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/IncomingPhoneNumbers/" <> sid <> ".json")

-- | Get an 'IncomingPhoneNumber' by 'PhoneNumberSID'.
get :: MonadThrow m => PhoneNumberSID -> TwilioT m IncomingPhoneNumber
get = Resource.get
