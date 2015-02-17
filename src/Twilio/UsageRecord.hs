{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageRecord
  ( -- * Resource
    UsageRecord(..)
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

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
    <*> (v .: "usage"      >>= safeRead)
    <*>  v .: "usage_unit"
    <*> (v .: "count"      <&> fmap safeRead
                           >>= maybeReturn')
    <*> (v .: "count_unit" <&> (=<<) filterEmpty)
    <*> (v .: "price"      >>= safeRead)
    <*>  v .: "price_unit"
    <*> (v .: "uri"        <&> parseRelativeReference
                           >>= maybeReturn)
  parseJSON _ = mzero
