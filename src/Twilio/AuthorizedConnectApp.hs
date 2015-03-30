{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.AuthorizedConnectApp
  ( -- * Resource
    AuthorizedConnectApp(..)
  , ConnectAppSID
  , Twilio.AuthorizedConnectApp.get
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseURI, parseRelativeReference)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data AuthorizedConnectApp = AuthorizedConnectApp
  { dateCreated  :: !UTCTime
  , dateUpdated  :: !UTCTime
  , accountSID   :: !AccountSID
--  , permissions  :: !Permissions
  , sid          :: !ConnectAppSID
  , friendlyName :: !String
  , description  :: !String
  , companyName  :: !String
  , homepageURL  :: !(Maybe URI)
  , uri          :: !URI
  } deriving (Show, Eq)

instance FromJSON AuthorizedConnectApp where
  parseJSON (Object v) = AuthorizedConnectApp
    <$> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
--    <*>  v .: "permissions"
    <*>  v .: "connect_app_sid"
    <*>  v .: "connect_app_friendly_name"
    <*>  v .: "connect_app_description"
    <*>  v .: "connect_app_company_name"
    <*> (v .: "connect_app_homepage_url" <&> fmap filterEmpty
                                         <&> fmap (fmap parseURI)
                                         >>= maybeReturn'')
    <*> (v .: "uri"                      <&> parseRelativeReference
                                         >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 ConnectAppSID AuthorizedConnectApp where
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    ("/AuthorizedConnectApps/" ++ sid ++ ".json")

-- | Get an 'AuthorizedConnectApp' by 'ConnectAppSID'.
get :: (MonadThrow m, MonadIO m) => ConnectAppSID -> TwilioT m AuthorizedConnectApp
get = Resource.get
