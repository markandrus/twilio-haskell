{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.OutgoingCallerIDs
  ( -- * Resource
    OutgoingCallerID(..)
  , PhoneNumberSID
    -- * List Resource
  , OutgoingCallerIDs(..)
  , get
  ) where

import Twilio.Client as Client
import Twilio.Types

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

get :: Client -> IO OutgoingCallerIDs
get client = runRequest client $
  Client.accountBaseURL (Client.accountSID client) ++ "/OutgoingCallerIds.json"

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
