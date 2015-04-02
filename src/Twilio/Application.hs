{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Application
  ( -- * Resource
    Application(..)
  , ApplicationSID
  , Twilio.Application.get
    -- * Types
  , Method(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Network.URI

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Application = Application
  { sid                   :: !ApplicationSID
  , dateCreated           :: !UTCTime
  , dateUpdated           :: !UTCTime
  , friendlyName          :: !Text
  , accountSID            :: !AccountSID
  , apiVersion            :: !APIVersion
  , voiceURL              :: !(Maybe URI)
  , voiceMethod           :: !(Maybe Method)
  , voiceFallbackURL      :: !(Maybe URI)
  , voiceFallbackMethod   :: !(Maybe Method)
  , statusCallback        :: !(Maybe URI)
  , statusCallbackMethod  :: !(Maybe Method)
  , voiceCallerIDLookup   :: !Bool
  , smsURL                :: !(Maybe URI)
  , smsMethod             :: !(Maybe Method)
  , smsFallbackURL        :: !(Maybe URI)
  , smsFallbackMethod     :: !(Maybe Method)
  , smsStatusCallback     :: !(Maybe URI)
  , messageStatusCallback :: !(Maybe URI)
  , uri                   :: !URI
  } deriving (Show, Eq)

instance FromJSON Application where
  parseJSON (Object v) = Application
    <$>  v .: "sid"
    <*> (v .: "date_created"            >>= parseDateTime)
    <*> (v .: "date_updated"            >>= parseDateTime)
    <*>  v .: "friendly_name"
    <*>  v .: "account_sid"
    <*>  v .: "api_version"
    <*> (v .: "voice_url"               <&> filterEmpty
                                        <&> fmap (parseURI . T.unpack)
                                        >>= maybeReturn')
    <*>  v .: "voice_method"
    <*> (v .: "voice_fallback_url"      <&> filterEmpty
                                        <&> fmap (parseURI . T.unpack)
                                        >>= maybeReturn')
    <*>  v .: "voice_fallback_method"
    <*> (v .: "status_callback"         <&> filterEmpty
                                        <&> fmap (parseURI . T.unpack)
                                        >>= maybeReturn')
    <*>  v .: "status_callback_method"
    <*>  v .: "voice_caller_id_lookup"
    <*> (v .: "sms_url"                 <&> filterEmpty
                                        <&> fmap (parseURI . T.unpack)
                                        >>= maybeReturn')
    <*>  v .: "sms_method"
    <*> (v .: "sms_fallback_url"        <&> filterEmpty
                                        <&> fmap (parseURI . T.unpack)
                                        >>= maybeReturn')
    <*>  v .: "sms_fallback_method"
    <*> (v .: "sms_status_callback"     <&> fmap filterEmpty
                                        <&> fmap (fmap $ parseURI . T.unpack)
                                        >>= maybeReturn'')
    <*> (v .: "message_status_callback" <&> fmap filterEmpty
                                        <&> fmap (fmap $ parseURI . T.unpack)
                                        >>= maybeReturn'')
    <*> (v .: "uri"                     <&> parseRelativeReference
                                        >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 ApplicationSID Application where
  get1 applicationSID = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Applications/" <> getSID applicationSID <> ".json")

-- | Get an 'Application' by 'ApplicationSID'.
get :: MonadThrow m => ApplicationSID -> TwilioT m Application
get = Resource.get

{- Types -}

data Method
  = GET
  | POST
  deriving (Show, Eq)

instance FromJSON Method where
  parseJSON (String "GET") = return GET
  parseJSON (String "POST") = return POST
  parseJSON _ = mzero
