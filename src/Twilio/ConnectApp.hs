{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.ConnectApp
  ( -- * Resource
    ConnectApp(..)
  , ConnectAppSID
  , Twilio.ConnectApp.get
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

data ConnectApp = ConnectApp
  { sid                       :: !ConnectAppSID
  , accountSID                :: !AccountSID
--  , permissions               :: !
  , friendlyName              :: !String
  , description               :: !String
  , companyName               :: !String
  , homepageURL               :: !(Maybe URI)
  , authorizeRedirectURL      :: !(Maybe URI)
  , deauthorizeCallbackURL    :: !(Maybe URI)
--  , deauthorizeCallbackMethod :: !String
  , uri                       :: !URI
  } deriving (Show, Eq)

instance FromJSON ConnectApp where
  parseJSON (Object v) = ConnectApp
    <$>  v .: "sid"
    <*>  v .: "account_sid"
--    <*>  v .: "permissions"
    <*>  v .: "friendly_name"
    <*>  v .: "description"
    <*>  v .: "company_name"
    <*> (v .: "homepage_url"             <&> fmap filterEmpty
                                         <&> fmap (fmap parseURI)
                                         >>= maybeReturn'')
    <*> (v .: "authorize_redirect_url"   <&> fmap filterEmpty
                                         <&> fmap (fmap parseURI)
                                         >>= maybeReturn'')
    <*> (v .: "deauthorize_callback_url" <&> fmap filterEmpty
                                         <&> fmap (fmap parseURI) 
                                         >>= maybeReturn'')
--    <*>  v .: "deauthorize_callback_method"
    <*> (v .: "uri"                      <&> parseRelativeReference
                                         >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 ConnectAppSID ConnectApp where
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<<
    makeTwilioRequest ("/ConnectApps/" ++ sid ++ ".json")

-- | Get a 'ConnectApp' by 'ConnectAppSID'.
get :: (MonadThrow m, MonadIO m) => ConnectAppSID -> TwilioT m ConnectApp
get = Resource.get
