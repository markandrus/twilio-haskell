{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.ConnectApp where

import Twilio.Account
import Twilio.Client as Client
import Twilio.Types

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseURI, parseRelativeReference)

connectApps :: Client -> IO ConnectApps
connectApps client = runRequest client $
  accountBaseURL (Client.accountSID client) ++ "/ConnectApps.json"

-- | Connect App 'SID's are 34 characters long and begin with \"CN\".
newtype ConnectAppSID = ConnectAppSID { getConnectAppSID :: String }
  deriving (Show, Eq)

instance SID ConnectAppSID where
  getSIDWrapper = wrap ConnectAppSID
  getPrefix = Const ('C', 'N')
  getSID = getConnectAppSID

instance FromJSON ConnectAppSID where
  parseJSON = parseJSONToSID

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
