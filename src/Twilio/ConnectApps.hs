{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.ConnectApps
  ( -- * Resource
    ConnectApps(..)
  , Twilio.ConnectApps.get
  ) where

import Twilio.Types
import Twilio.ConnectApp

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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

instance Get0 ConnectApps where
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest "/ConnectApps.json"

-- | Get 'ConnectApps'.
get :: (MonadThrow m, MonadIO m) => TwilioT m ConnectApps
get = Resource.get
