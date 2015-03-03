{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageTrigger
  ( -- * Resource
    UsageTrigger(..)
  , get
  , get'
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Network.URI

{- Resource -}

data UsageTrigger = UsageTrigger
  { sid            :: !UsageTriggerSID
  , dateCreated    :: !UTCTime
  , dateUpdated    :: !UTCTime
  , accountSID     :: !AccountSID
  , friendlyName   :: !String
  , recurring      :: !(Maybe String)
  , usageCategory  :: !String
  , triggerBy      :: !String
  , triggerValue   :: !String
  , currentValue   :: !String
  , usageRecordURI :: !URI
  , callbackURL    :: !(Maybe String)
  , callbackMethod :: !String
  , dateFired      :: !(Maybe String)
  , uri            :: !URI
  } deriving (Eq, Show)

instance FromJSON UsageTrigger where
  parseJSON (Object v) = UsageTrigger
    <$>  v .: "sid"
    <*> (v .: "date_created"     >>= parseDateTime)
    <*> (v .: "date_updated"     >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "friendly_name"
    <*>  v .: "recurring"
    <*>  v .: "usage_category"
    <*>  v .: "trigger_by"
    <*>  v .: "trigger_value"
    <*>  v .: "current_value"
    <*> (v .: "usage_record_uri" <&> parseRelativeReference
                                 >>= maybeReturn)
    <*>  v .: "callback_url"
    <*>  v .: "callback_method"
    <*>  v .: "date_fired"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
  parseJSON _ = mzero

-- | Get a 'UsageTrigger' by 'UsageTriggerSID'.
get :: (MonadThrow m, MonadIO m) => UsageTriggerSID -> TwilioT m UsageTrigger
get usageTriggerSID
  = requestForAccount $ "/Usage/Triggeres/" ++ getSID usageTriggerSID ++ ".json"

-- | Get a 'UsageTrigger' for an account by 'UsageTriggerSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> UsageTriggerSID
     -> TwilioT m UsageTrigger
get' accountSID usageTriggerSID = forAccount accountSID $ get usageTriggerSID
