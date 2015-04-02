{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.AuthorizedConnectApps
  ( -- * Resource
    AuthorizedConnectApps(..)
  , Twilio.AuthorizedConnectApps.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.AuthorizedConnectApp
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

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

instance Get0 AuthorizedConnectApps where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/AuthorizedConnectApps.json"

-- | Get 'AuthorizedConnectApps'.
get :: MonadThrow m => TwilioT m AuthorizedConnectApps
get = Resource.get
