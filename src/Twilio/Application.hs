{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Application
  ( -- * Resource
    Application(..)
  , ApplicationSID
  , get
  , get'
    -- * Types
  , Method(..)
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseURI, parseRelativeReference)

{- Resource -}

data Application = Application
  { sid                   :: !ApplicationSID
  , dateCreated           :: !UTCTime
  , dateUpdated           :: !UTCTime
  , friendlyName          :: !String
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
                                        <&> fmap parseURI
                                        >>= maybeReturn')
    <*>  v .: "voice_method"
    <*> (v .: "voice_fallback_url"      <&> filterEmpty
                                        <&> fmap parseURI
                                        >>= maybeReturn')
    <*>  v .: "voice_fallback_method"
    <*> (v .: "status_callback"         <&> filterEmpty
                                        <&> fmap parseURI
                                        >>= maybeReturn')
    <*>  v .: "status_callback_method"
    <*>  v .: "voice_caller_id_lookup"
    <*> (v .: "sms_url"                 <&> filterEmpty
                                        <&> fmap parseURI
                                        >>= maybeReturn')
    <*>  v .: "sms_method"
    <*> (v .: "sms_fallback_url"        <&> filterEmpty
                                        <&> fmap parseURI
                                        >>= maybeReturn')
    <*>  v .: "sms_fallback_method"
    <*> (v .: "sms_status_callback"     <&> fmap filterEmpty
                                        <&> fmap (fmap parseURI)
                                        >>= maybeReturn'')
    <*> (v .: "message_status_callback" <&> fmap filterEmpty
                                        <&> fmap (fmap parseURI)
                                        >>= maybeReturn'')
    <*> (v .: "uri"                     <&> parseRelativeReference
                                        >>= maybeReturn)
  parseJSON _ = mzero

-- | Get an 'Application' by 'ApplicationSID'.
get :: (MonadThrow m, MonadIO m) => ApplicationSID -> TwilioT m Application
get applicationSID = requestForAccount
                   $ "/Applications/" ++ getSID applicationSID ++ ".json"

-- | Get an account's 'Application' by 'ApplicationSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> ApplicationSID
     -> TwilioT m Application
get' accountSID applicationSID = forAccount accountSID $ get applicationSID

{- Types -}

data Method
  = GET
  | POST
  deriving (Show, Eq)

instance FromJSON Method where
  parseJSON (String "GET") = return GET
  parseJSON (String "POST") = return POST
  parseJSON _ = mzero
