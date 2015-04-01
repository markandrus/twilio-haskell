{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.IncomingPhoneNumber
  ( -- * Resource
    IncomingPhoneNumber(..)
  , Twilio.IncomingPhoneNumber.get
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join, mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

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
  , friendlyName         :: !String
  , accountSID           :: !AccountSID
  , phoneNumber          :: !String
  , apiVersion           :: !APIVersion
  , voiceCallerIDLookup  :: !Bool
  , voiceURL             :: !(Maybe String)
  , voiceMethod          :: !String
  , voiceFallbackURL     :: !(Maybe String)
  , voiceFallbackMethod  :: !String
  , statusCallback       :: !(Maybe String)
  , statusCallbackMethod :: !String
  , voiceApplicationSID  :: !(Maybe ApplicationSID)
  , smsURL               :: !(Maybe String)
  , smsMethod            :: !String
  , smsFallbackURL       :: !(Maybe String)
  , smsFallbackMethod    :: !String
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
    <*> (v .: "voice_url" <&> getNonEmptyString)
    <*>  v .: "voice_method"
    <*> (v .: "voice_fallback_url" <&> (join . fmap getNonEmptyString))
    <*>  v .: "voice_fallback_method"
    <*> (v .: "status_callback"  <&> getNonEmptyString)
    <*>  v .: "status_callback_method"
    <*> (v .: "voice_application_sid" <&> (join . fmap parseSID . getNonEmptyString))
    <*> (v .: "sms_url"          <&> getNonEmptyString)
    <*>  v .: "sms_method"
    <*> (v .: "sms_fallback_url" <&> getNonEmptyString)
    <*>  v .: "sms_fallback_method"
    <*> (v .: "sms_application_sid" <&> (join . fmap parseSID . getNonEmptyString))
    <*> (v .: "capabilities" >>= parseJSON)
    <*>  v .: "address_requirements"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 PhoneNumberSID IncomingPhoneNumber where
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    ("/IncomingPhoneNumbers/" ++ sid ++ ".json")

-- | Get an 'IncomingPhoneNumber' by 'PhoneNumberSID'.
get :: (MonadThrow m, MonadIO m) => PhoneNumberSID -> TwilioT m IncomingPhoneNumber
get = Resource.get
