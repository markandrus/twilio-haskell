{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.ShortCode
  ( -- * Resource
    ShortCode(..)
  , Twilio.ShortCode.get
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

data ShortCode = ShortCode
  { sid               :: !ShortCodeSID
  , dateCreated       :: !UTCTime
  , dateUpdated       :: !UTCTime
  , friendlyName      :: !Text
  , accountSID        :: !AccountSID
  , shortCode         :: !Text
  , smsURL            :: !(Maybe Text)
  , smsMethod         :: !Text
  , smsFallbackURL    :: !(Maybe Text)
  , smsFallbackMethod :: !Text
  , uri               :: !URI
  } deriving (Eq, Show)

instance FromJSON ShortCode where
  parseJSON (Object v) = ShortCode
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "friendly_name"
    <*>  v .: "account_sid"
    <*>  v .: "short_code"
    <*> (v .: "sms_url" <&> getNonEmptyText)
    <*>  v .: "sms_method"
    <*> (v .: "sms_fallback_url" <&> getNonEmptyText)
    <*>  v .: "sms_fallback_method"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 ShortCodeSID ShortCode where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/SMS/ShortCodes/" <> sid <> ".json")

-- | Get a 'ShortCode' by 'ShortCodeSID'.
get :: MonadThrow m => ShortCodeSID -> TwilioT m ShortCode
get = Resource.get
