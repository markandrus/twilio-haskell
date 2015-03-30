{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.OutgoingCallerID
  ( -- * Resource
    OutgoingCallerID(..)
  , PhoneNumberSID
  , Twilio.OutgoingCallerID.get
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data OutgoingCallerID = OutgoingCallerID
  { sid          :: !PhoneNumberSID
  , dateCreated  :: !UTCTime
  , dateUpdated  :: !UTCTime
  , friendlyName :: !String
  , accountSID   :: !AccountSID
  , phoneNumber  :: !String
  , uri          :: !URI
  } deriving (Show, Eq)

instance FromJSON OutgoingCallerID where
  parseJSON (Object v) = OutgoingCallerID
    <$>  v .: "sid"
    <*> (v .: "date_created"  >>= parseDateTime)
    <*> (v .: "date_updated"  >>= parseDateTime)
    <*>  v .: "friendly_name"
    <*>  v .: "account_sid"
    <*>  v .: "phone_number"
    <*> (v .: "uri"           <&> parseRelativeReference
                              >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 PhoneNumberSID OutgoingCallerID where
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    ("/OutgoingCallerIds/" ++ sid ++ ".json")

-- | Get an 'OutgoingCallerID' by 'PhoneNumberSID'
get :: (MonadThrow m, MonadIO m)
    => PhoneNumberSID
    -> TwilioT m OutgoingCallerID
get = Resource.get
