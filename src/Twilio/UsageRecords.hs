{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageRecords
  ( -- * Resource
    UsageRecords(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.UsageRecord

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

{- Resource -}

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

-- | Get 'UsageRecords'.
get :: (MonadThrow m, MonadIO m) => TwilioT m UsageRecords
get = requestForAccount "/Usage/Records.json"

-- | Get an account's 'UsageRecords'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m UsageRecords
get' = flip forAccount get
