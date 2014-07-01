{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.ConnectApps
  ( -- * Resource
    ConnectApps(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.ConnectApp hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

{- Resource -}

data ConnectApps = ConnectApps
  { connectAppsPagingInformation :: PagingInformation
  , connectAppList :: [ConnectApp]
  } deriving (Show, Eq)

instance List ConnectApps ConnectApp where
  getListWrapper = wrap (ConnectApps . fromJust)
  getList = connectAppList
  getPlural = Const "connect_apps"

instance FromJSON ConnectApps where
  parseJSON = parseJSONToList

-- | Get 'ConnectApps'.
get :: (MonadThrow m, MonadIO m) => TwilioT m ConnectApps
get = requestForAccount "/ConnectApps.json"

-- | Get an account's 'ConnectApps'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m ConnectApps
get' = flip forAccount get
