{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.APIKey
  ( -- * Resource
    APIKey(..)
  , APIKeySID
  , Twilio.APIKey.get
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

data APIKey = APIKey
  { sid :: !APIKeySID
  , friendlyName :: !Text
 , secret       :: !(Maybe Text)
  , dateCreated  :: !UTCTime
  , dateUpdated  :: !UTCTime
  } deriving (Show, Eq, Ord)

instance FromJSON APIKey where
  parseJSON (Object v) = APIKey
    <$>  v .:  "sid"
    <*>  v .:  "friendly_name"
    <*>  v .:? "secret"
    <*> (v .:  "date_created" >>= parseDateTime)
    <*> (v .:  "date_updated" >>= parseDateTime)
  parseJSON _ = mzero

instance Get1 APIKeySID APIKey where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Keys/" <> sid <> ".json")

get :: MonadThrow m => APIKeySID -> TwilioT m APIKey
get = Resource.get
