{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Calls
  ( get
  , Call(..)
  , CallSID
  , Calls(..)
  ) where

import Twilio.Account
import Twilio.Client as Client
import Twilio.PhoneNumber
import Twilio.Types

import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), Const(..))
import Data.Aeson
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

get :: Twilio Calls
get = do
  (accountSID, _) <- ask
  request $ Client.accountBaseURL accountSID ++ "/Calls.json"

data Call = Call
  { sid            :: !CallSID
  , parentCallSID  :: !(Maybe CallSID)
  , dateCreated    :: !UTCTime
  , dateUpdated    :: !UTCTime
  , accountSID     :: !AccountSID
  , to             :: !(Maybe String)
  , from           :: !String
  , phoneNumberSID :: !(Maybe PhoneNumberSID)
  , status         :: !CallStatus
  , startTime      :: !UTCTime
  , endTime        :: !(Maybe UTCTime)
  , duration       :: !(Maybe Int)
  , price          :: !(Maybe Double)
  , priceUnit      :: !(Maybe PriceUnit)
  , direction      :: !CallDirection
  , answeredBy     :: !(Maybe AnsweredBy)
  , forwardedFrom  :: !(Maybe String)
  , callerName     :: !(Maybe String)
  , uri            :: !URI
  , apiVersion     :: !APIVersion
  } deriving (Show, Eq)

maybeParseSID str = case parseSID str of
  Left  _   -> Nothing
  Right sid -> Just sid

instance FromJSON Call where
  parseJSON (Object v) = Call
    <$>  v .: "sid"
    <*>  v .: "parent_call_sid"
    <*> (v .: "date_created"     >>= parseDateTime)
    <*> (v .: "date_updated"     >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "to"               <&> filterEmpty
    <*>  v .: "from"
    <*> (v .: "phone_number_sid" <&> filterEmpty
                                 <&> (=<<) maybeParseSID)
    <*>  v .: "status"
    <*> (v .: "start_time"       >>= parseDateTime)
    <*> (v .: "end_time"         <&> (=<<) parseDateTime)
    <*> (v .: "duration"         <&> fmap safeRead
                                 >>= maybeReturn')
    <*> (v .: "price"            <&> fmap safeRead
                                 >>= maybeReturn')
    <*>  v .: "price_unit"
    <*>  v .: "direction"
    <*>  v .: "answered_by"
    <*>  v .: "forwarded_from"   <&> filterEmpty
    <*>  v .: "caller_name"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
    <*>  v .: "api_version"
  parseJSON _ = mzero

data Calls = Calls
  { callsPagingInformation :: PagingInformation
  , callList :: [Call]
  } deriving (Show, Eq)

instance List Calls Call where
  getListWrapper = wrap (Calls . fromJust)
  getList = callList
  getPlural = Const "calls"

instance FromJSON Calls where
  parseJSON = parseJSONToList
