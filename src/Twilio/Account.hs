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

data Account = Account
  { sid             :: !AccountSID
  , dateCreated     :: !UTCTime
  , dateUpdated     :: !UTCTime
  , friendlyName    :: !Text
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
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest'
    ("/Accounts/" <> sid <> ".json")
  {-
  get1 (getSID -> sid) =
    let twilioRequest = makeTwilioRequest' $ "/Accounts/" ++ sid ++ ".json"
    in  twilioRequest >>= request parseJSONFromResponse
  -}

get :: MonadThrow m => AccountSID -> TwilioT m Account
get = Resource.get

-- instance Post2 AccountSID () () where
--   post2 accountSID () = return ()

-- | Suspends a subaccount by POST-ing the parameter 'status' with the value
-- 'Suspended'.
suspend :: Monad m => AccountSID -> TwilioT m ()
suspend = undefined -- flip Resource.post ()

-- Reactivates a suspended subaccount by POST-ing the parameter 'status' with
-- the value 'Active'.
unsuspend :: Monad m => AccountSID -> TwilioT m ()
unsuspend = undefined -- flip Resource.post ()

-- Closes a subaccount by POST-ing the parameter 'status' with the value
-- 'Closed'.
close :: Monad m => AccountSID -> TwilioT m ()
close = undefined -- flip Resource.post ()

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
