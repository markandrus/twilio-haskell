{-# LANGUAGE OverloadedStrings #-}

module Call
  ( calls
  , Call(..)
  , CallSID
  ) where

import Account
import Client
import PhoneNumber
import Types

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Char (isLower, isNumber)
import Data.Maybe
import Data.Text (unpack)
import Network.HTTP.Client

calls :: Client -> Request
calls client = fromJust $ do
  let accountSID = Client.accountSID client
      authToken  = getAuthToken $ Client.authToken client
  req <- parseUrl $ accountBaseURL accountSID ++ "/Calls.json"
  return $ asClient client req

-- call :: Client -> SID -> Request Call
-- call = undefined

data Call = Call
  { sid            :: !CallSID
  , parentCallSID  :: !CallSID
  , dateCreated    :: !String
  , dateUpdated    :: !String
  , accountSID     :: !AccountSID
  , to             :: !String
  , from           :: !String
  , phoneNumberSID :: !(Maybe PhoneNumberSID)
  } deriving (Show, Eq)

instance FromJSON Call where
  parseJSON (Object v)
    =  Call
   <$> v .: "sid"
   <*> v .: "parent_call_sid"
   <*> v .: "date_created"
   <*> v .: "date_updated"
   <*> v .: "account_sid"
   <*> v .: "to"
   <*> v .: "from"
   <*> v .: "phone_number_sid"
  parseJSON _ = mzero

-- | Call 'SID's are 34 characters long and begin with \"CA\".
newtype CallSID = CallSID { getCallSID :: String }
  deriving (Show, Eq)

instance SID CallSID where
  getSIDWrapper = wrap CallSID
  getPrefix = Const ('C', 'A')
  getSID = getCallSID

instance FromJSON CallSID where
  parseJSON = parseJSONToSID
