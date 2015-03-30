{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.AuthorizedConnectApps
  ( -- * Resource
    AuthorizedConnectApps(..)
  , Twilio.AuthorizedConnectApps.get
  ) where

import Twilio.Types
import Twilio.AuthorizedConnectApp hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/AuthorizedConnectApps.json"

-- | Get 'AuthorizedConnectApps'.
get :: (MonadThrow m, MonadIO m) => TwilioT m AuthorizedConnectApps
get = Resource.get
