{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Usage where

import qualified Twilio.Client as Client
import Twilio.Types

import Control.Applicative ((<$>), (<*>), Const(Const))
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

data UsageRecord = UsageRecord
  { category    :: !String
  , description :: !String
  , accountSID  :: !AccountSID
  , startDate   :: !UTCTime
  , endDate     :: !UTCTime
  , usage       :: !Integer
  , usageUnit   :: !String
  , count       :: !String
  , countUnit   :: !String
  , price       :: !Double
  , priceUnit   :: !PriceUnit
  , uri         :: !URI
  } deriving (Show, Eq)

instance FromJSON UsageRecord where
  parseJSON (Object v) = UsageRecord
    <$>  v .: "category"
    <*>  v .: "description"
    <*>  v .: "account_sid"
    <*> (v .: "start_date" >>= parseDateTime)
    <*> (v .: "end_date"   >>= parseDateTime)
    <*>  v .: "usage"
    <*>  v .: "usage_unit"
    <*>  v .: "count"
    <*>  v .: "count_unit"
    <*>  v .: "price"
    <*>  v .: "price_unit"
    <*> (v .: "uri"        <&> parseRelativeReference
                           >>= maybeReturn)
  parseJSON _ = mzero

data UsageRecords = UsageRecords
  { usageRecordsPagingInformation :: PagingInformation
  , usageRecordList :: [UsageRecord]
  } deriving (Show, Eq)

instance List UsageRecords UsageRecord where
  getListWrapper = wrap (UsageRecords . fromJust)
  getList = usageRecordList
  getPlural = Const "usage_records"

instance FromJSON UsageRecords where
  parseJSON = parseJSONToList

usageRecords :: Client.Client -> IO UsageRecords
usageRecords client = Client.runRequest client $
  Client.accountBaseURL (Client.accountSID client) ++ "/Usage/Records.json"
