{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Applications
  ( -- * Resource
    Application(..)
  , ApplicationSID
    -- * List Resource
  , Applications(..)
  , get
  , get'
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>), Const(..))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseURI, parseRelativeReference)

-- | Get the 'Applications' for your account.
get :: (MonadThrow m, MonadIO m) => TwilioT m Applications
get = request "/Applications.json"

-- | Get the 'Applications' for a sub-account of your account.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m Applications
get' = flip forSubAccount get

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

data Method
  = GET
  | POST
  deriving (Show, Eq)

instance FromJSON Method where
  parseJSON (String "GET") = return GET
  parseJSON (String "POST") = return POST
  parseJSON _ = mzero

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

data Applications = Applications
  { applicationsPagingInformation :: PagingInformation
  , applicationList :: [Application]
  } deriving (Show, Eq)

instance List Applications Application where
  getListWrapper = wrap (Applications . fromJust)
  getList = applicationList
  getPlural = Const "applications"

instance FromJSON Applications where
  parseJSON = parseJSONToList
