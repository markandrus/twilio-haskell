{-#LANGUAGE OverloadedStrings #-}

module Twilio.AuthorizedConnectApp
  ( -- * Resource
    AuthorizedConnectApp(..)
  , ConnectAppSID
  , get
  , get'
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseURI, parseRelativeReference)

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

-- | Get a 'ConnectApp' by 'ConnectAppSID'.
get :: (MonadThrow m, MonadIO m) => ConnectAppSID -> TwilioT m AuthorizedConnectApp
get connectAppSID
  = requestForAccount $ "/AuthorizedConnectApps/" ++ getSID connectAppSID ++ ".json"

-- | Get an account's 'ConnectApp' by 'ConnectAppSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> ConnectAppSID
     -> TwilioT m AuthorizedConnectApp
get' accountSID connectAppSID = forAccount accountSID $ get connectAppSID
