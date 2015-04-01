{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageRecord
  ( -- * Resource
    UsageRecord(..)
  ) where

import Control.Applicative
import Control.Error.Safe
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Network.URI

import Twilio.Types
import Twilio.Internal.Parser

{- Resource -}

data UsageRecord = UsageRecord
  { category    :: !String
  , description :: !String
  , accountSID  :: !AccountSID
  , startDate   :: !UTCTime
  , endDate     :: !UTCTime
  , usage       :: !Double
  , usageUnit   :: !String
  , count       :: !(Maybe Double)
  , countUnit   :: !(Maybe String)
  , price       :: !Double
  , priceUnit   :: !PriceUnit
  , uri         :: !URI
  } deriving (Show, Eq)

instance FromJSON UsageRecord where
  parseJSON (Object v) = UsageRecord
    <$>  v .: "category"
    <*>  v .: "description"
    <*>  v .: "account_sid"
    <*> (v .: "start_date" >>= parseDate)
    <*> (v .: "end_date"   >>= parseDate)
    <*> (v .: "usage"      >>= readZ)
    <*>  v .: "usage_unit"
    <*> (v .: "count"      <&> fmap readZ
                           >>= maybeReturn')
    <*> (v .: "count_unit" <&> (=<<) filterEmpty)
    <*> (v .: "price"      >>= readZ)
    <*>  v .: "price_unit"
    <*> (v .: "uri"        <&> parseRelativeReference
                           >>= maybeReturn)
  parseJSON _ = mzero
