{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Account
  ( AccountSID
  , AuthToken(getAuthToken)
  , parseAuthToken
  , Account(..)
  , Accounts
  , accounts
  ) where

import Twilio.Client (Client(accountSID), baseURL, runRequest)
import Twilio.Types

import Control.Applicative ((<$>), (<*>), Const(..))
import Control.Monad (mzero)
import Data.Aeson
import Data.Char (isLower, isNumber)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

data AccountStatus
  = Active
  | Suspended
  | Closed
  deriving Eq

instance Show AccountStatus where
  show Active    = "active"
  show Suspended = "suspended"
  show Closed    = "closed"

instance FromJSON AccountStatus where
  parseJSON (String "active")    = return Active
  parseJSON (String "suspended") = return Suspended
  parseJSON (String "closed")    = return Closed
  parseJSON _ = mzero

data Type
  = Full
  | Trial
  deriving (Show, Read, Eq)

instance FromJSON Type where
  parseJSON (String "Full")  = return Full
  parseJSON (String "Trial") = return Trial

data Account = Account
  { sid             :: !AccountSID
  , dateCreated     :: !UTCTime
  , dateUpdated     :: !UTCTime
  , friendlyName    :: !String
  , type'           :: !Type
  , status          :: !AccountStatus
  , authToken       :: !AuthToken
  , uri             :: !URI
  , ownerAccountSID :: !(Maybe AccountSID)
  } deriving (Show, Eq)

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
  { accountsPagingInformation :: PagingInformation
  , accountList :: [Account]
  } deriving (Show, Eq)

instance List Accounts Account where
  getListWrapper = wrap (Accounts . fromJust)
  getList = accountList
  getPlural = Const "accounts"

instance FromJSON Accounts where
  parseJSON = parseJSONToList

accounts :: Client -> IO Accounts
accounts client = runRequest client $ baseURL ++ "/Accounts.json"
