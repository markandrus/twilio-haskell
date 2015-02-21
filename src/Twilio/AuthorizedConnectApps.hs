{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.AuthorizedConnectApps
  ( -- * Resource
    AuthorizedConnectApps(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.AuthorizedConnectApp hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

{- Resource -}

data AuthorizedConnectApps = AuthorizedConnectApps
  { authorizedConnectAppsPagingInformation :: PagingInformation
  , authorizedConnectAppList :: [AuthorizedConnectApp]
  } deriving (Show, Eq)

instance List AuthorizedConnectApps AuthorizedConnectApp where
  getListWrapper = wrap (AuthorizedConnectApps . fromJust)
  getList = authorizedConnectAppList
  getPlural = Const "authorized_connect_apps"

instance FromJSON AuthorizedConnectApps where
  parseJSON = parseJSONToList

-- | Get 'AuthorizedConnectApps'.
get :: (MonadThrow m, MonadIO m) => TwilioT m AuthorizedConnectApps
get = requestForAccount "/AuthorizedConnectApps.json"

-- | Get an account's 'AuthorizedConnectApps'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m AuthorizedConnectApps
get' = flip forAccount get
