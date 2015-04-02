{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.ConnectApps
  ( -- * Resource
    ConnectApps(..)
  , Twilio.ConnectApps.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.ConnectApp
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

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
  get0 = request parseJSONFromResponse =<< makeTwilioRequest "/ConnectApps.json"

-- | Get 'ConnectApps'.
get :: MonadThrow m => TwilioT m ConnectApps
get = Resource.get
