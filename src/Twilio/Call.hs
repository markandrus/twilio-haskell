{-#LANGUAGE InstanceSigs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Call
  ( calls
  , Call(..)
  , CallSID
  , Calls(..)
  ) where

import Twilio.Account
import Twilio.Client as Client
import Twilio.PhoneNumber
import Twilio.Types

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
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
  , parentCallSID  :: !(Maybe CallSID)
  , dateCreated    :: !String
  , dateUpdated    :: !String
  , accountSID     :: !AccountSID
  -- NOTE: Sometimes Twilio sends "" instead of `null`.
  , to             :: !String
  , from           :: !String
  -- NOTE: Sometimes Twilio sends "" instead of `null`.
  , phoneNumberSID :: !String -- !(Maybe PhoneNumberSID)
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

data Calls = Calls
  { callsPagingInformation :: PagingInformation
  , callList :: [Call]
  } deriving (Show, Eq)

instance List Calls Call where
  getListWrapper = wrap Calls
  getPagingInformation = callsPagingInformation
  getItems = callList
  getPlural = Const "calls"

instance FromJSON Calls where
  parseJSON :: Value -> Parser Calls
  parseJSON = parseJSONToList
