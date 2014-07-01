{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.OutgoingCallerIDs
  ( -- * Resource
    OutgoingCallerID(..)
  , PhoneNumberSID
    -- * List Resource
  , OutgoingCallerIDs(..)
  , get
  , get'
  ) where

import Twilio.Types

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

get :: (MonadThrow m, MonadIO m) => TwilioT m OutgoingCallerIDs
get = requestForAccount "/OutgoingCallerIds.json"

get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m OutgoingCallerIDs
get' = flip forAccount get

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

data OutgoingCallerIDs = OutgoingCallerIDs
  { outgoingCallerIDsPagingInformation :: !PagingInformation
  , outgoingCallerIDList :: [OutgoingCallerID]
  } deriving (Show, Eq)

instance List OutgoingCallerIDs OutgoingCallerID where
  getListWrapper = wrap (OutgoingCallerIDs . fromJust)
  getList = outgoingCallerIDList
  getPlural = Const "outgoing_caller_ids"

instance FromJSON OutgoingCallerIDs where
  parseJSON = parseJSONToList
