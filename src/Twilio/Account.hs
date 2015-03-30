{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Account
  ( -- * Resource
    Account(..)
  , AccountSID
  , Twilio.Account.get
  , suspend
  , unsuspend
  , close
    -- * Types
  , Status(..)
  , Type(..)
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Network.URI

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

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

instance Get1 AccountSID Account where
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest'
    ("/Accounts/" ++ sid ++ ".json")

get :: Monad m => AccountSID -> TwilioT m Account
get = Resource.get

instance Post2 AccountSID () () where
  post2 accountSID () = return ()

-- | Suspends a subaccount by POST-ing the parameter 'status' with the value
-- 'Suspended'.
suspend :: Monad m => AccountSID -> TwilioT m ()
suspend = flip Resource.post ()

-- Reactivates a suspended subaccount by POST-ing the parameter 'status' with
-- the value 'Active'.
unsuspend :: Monad m => AccountSID -> TwilioT m ()
unsuspend = flip Resource.post ()

-- Closes a subaccount by POST-ing the parameter 'status' with the value
-- 'Closed'.
close :: Monad m => AccountSID -> TwilioT m ()
close = flip Resource.post ()

{- Types -}

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
