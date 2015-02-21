{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.IncomingPhoneNumber
  ( -- * Resource
    IncomingPhoneNumber(..)
  , get
  , get'
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join, mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

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
    <*> (v .: "capabilities" <&> parseCapabilitiesFromJSON)
    <*>  v .: "address_requirements"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
  parseJSON _ = mzero

-- | Get an 'IncomingPhoneNumber' by 'PhoneNumberSID'.
get :: (MonadThrow m, MonadIO m) => PhoneNumberSID -> TwilioT m IncomingPhoneNumber
get phoneNumberSID = requestForAccount $ "/IncomingPhoneNumbers/" ++ getSID phoneNumberSID ++ ".json"

-- | Get an 'IncomingPhoneNumber' for an account by 'PhoneNumberSID'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> PhoneNumberSID -> TwilioT m IncomingPhoneNumber
get' accountSID phoneNumberSID = forAccount accountSID $ get phoneNumberSID
