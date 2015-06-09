{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Message.Media
  ( -- * Resource
    Media(..)
    -- * Types
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Network.URI

import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Media = Media
  { sid         :: !MediaSID
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  , accountSID  :: !AccountSID
  , parentSID   :: !MessageSID
  , contentType :: !Text
  , uri         :: !URI
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance FromJSON Media where
  parseJSON (Object v) = Media
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "parent_sid"
    <*>  v .: "content_type"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

instance Get2 MessageSID MediaSID Media where
  get2 (getSID -> messageSID) (getSID -> mediaSID) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Messages/" <> messageSID <> "/Media/" <> mediaSID <> ".json")
