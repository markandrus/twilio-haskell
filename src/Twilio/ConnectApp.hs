{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.ConnectApp
  ( -- * Resource
    ConnectApp(..)
  , ConnectAppSID
  , Twilio.ConnectApp.get
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data ConnectApp = ConnectApp
  { sid                       :: !ConnectAppSID
  , accountSID                :: !AccountSID
--  , permissions               :: !
  , friendlyName              :: !Text
  , description               :: !Text
  , companyName               :: !Text
  , homepageURL               :: !(Maybe URI)
  , authorizeRedirectURL      :: !(Maybe URI)
  , deauthorizeCallbackURL    :: !(Maybe URI)
--  , deauthorizeCallbackMethod :: !Text
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
                                         <&> fmap (fmap $ parseURI . T.unpack)
                                         >>= maybeReturn'')
    <*> (v .: "authorize_redirect_url"   <&> fmap filterEmpty
                                         <&> fmap (fmap $ parseURI . T.unpack)
                                         >>= maybeReturn'')
    <*> (v .: "deauthorize_callback_url" <&> fmap filterEmpty
                                         <&> fmap (fmap $ parseURI . T.unpack)
                                         >>= maybeReturn'')
--    <*>  v .: "deauthorize_callback_method"
    <*> (v .: "uri"                      <&> parseRelativeReference
                                         >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 ConnectAppSID ConnectApp where
  get1 (getSID -> sid) = request parseJSONFromResponse =<<
    makeTwilioRequest ("/ConnectApps/" <> sid <> ".json")

-- | Get a 'ConnectApp' by 'ConnectAppSID'.
get :: MonadThrow m => ConnectAppSID -> TwilioT m ConnectApp
get = Resource.get
