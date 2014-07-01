{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Accounts
  ( -- * Resource
    Account(..)
  , AccountSID
    -- * List Resource
  , Accounts(..)
  , get
    -- * Types
  , Status(..)
  , Type(..)
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>), Const(..))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

data Status
  = Active
  | Suspended
  | Closed
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

{-
instance Show Status where
  show Active    = "active"
  show Suspended = "suspended"
  show Closed    = "closed"
-}

instance FromJSON Status where
  parseJSON (String "active")    = return Active
  parseJSON (String "suspended") = return Suspended
  parseJSON (String "closed")    = return Closed
  parseJSON _ = mzero

data Type
  = Full
  | Trial
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON Type where
  parseJSON (String "Full")  = return Full
  parseJSON (String "Trial") = return Trial
  parseJSON _ = mzero

data Account = Account
  { sid             :: !AccountSID
  , dateCreated     :: !UTCTime
  , dateUpdated     :: !UTCTime
  , friendlyName    :: !String
  , type'           :: !Type
  , status          :: !Status
  , authToken       :: !AuthToken
  , uri             :: !URI
  , ownerAccountSID :: !(Maybe AccountSID)
  } deriving (Show, Eq, Ord)

instance FromJSON Account where
  parseJSON (Object v) = Account
    <$>  v .: "sid"
    <*> (v .: "date_created"      >>= parseDateTime)
    <*> (v .: "date_updated"      >>= parseDateTime)
    <*>  v .: "friendly_name"
    <*>  v .: "type"
    <*>  v .: "status"
    <*>  v .: "auth_token"
    <*> (v .: "uri"               <&> parseRelativeReference
                                  >>= maybeReturn)
    <*>  v .: "owner_account_sid"
  parseJSON _ = mzero

data Accounts = Accounts
  { pagingInformation :: !PagingInformation
  , list              :: ![Account]
  } deriving (Show, Eq, Ord)

instance List Accounts Account where
  getListWrapper = wrap (Accounts . fromJust)
  getList = list
  getPlural = Const "accounts"

instance FromJSON Accounts where
  parseJSON = parseJSONToList

get :: (MonadThrow m, MonadIO m) => TwilioT m Accounts
get = request "/Accounts.json"
