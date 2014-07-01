{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.ConnectApp
  ( -- * Resource
    ConnectApp(..)
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

data ConnectApp = ConnectApp
  { sid                       :: !ConnectAppSID
  , dateCreated               :: !UTCTime
  , dateUpdated               :: !UTCTime
  , accountSID                :: !AccountSID
--  , permissions               :: !
  , friendlyName              :: !String
  , description               :: !String
  , companyName               :: !String
  , homepageURL               :: !(Maybe URI)
  , authorizedRedirectURL     :: !(Maybe URI)
  , deauthorizeCallbackURL    :: !(Maybe URI)
--  , deauthorizeCallbackMethod :: !String
  , uri                       :: !URI
  } deriving (Show, Eq)

instance FromJSON ConnectApp where
  parseJSON (Object v) = ConnectApp
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*>  v .: "account_sid"
--    <*>  v .: "permissions"
    <*>  v .: "friendly_name"
    <*>  v .: "description"
    <*>  v .: "company_name"
    <*> (v .: "homepage_url"             <&> fmap filterEmpty
                                         <&> fmap (fmap parseURI)
                                         >>= maybeReturn'')
    <*> (v .: "authorized_redirect_url"  <&> fmap filterEmpty
                                         <&> fmap (fmap parseURI)
                                         >>= maybeReturn'')
    <*> (v .: "deauthorize_callback_url" <&> fmap filterEmpty
                                         <&> fmap (fmap parseURI) 
                                         >>= maybeReturn'')
--    <*>  v .: "deauthorize_callback_method"
    <*> (v .: "uri"                      <&> parseRelativeReference
                                         >>= maybeReturn)
  parseJSON _ = mzero

-- | Get a 'ConnectApp' by 'ConnectAppSID'.
get :: (MonadThrow m, MonadIO m) => ConnectAppSID -> TwilioT m ConnectApp
get connectAppSID
  = requestForAccount $ "/ConnectApps/" ++ getSID connectAppSID ++ ".json"

-- | Get an account's 'ConnectApp' by 'ConnectAppSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> ConnectAppSID
     -> TwilioT m ConnectApp
get' accountSID connectAppSID = forAccount accountSID $ get connectAppSID
