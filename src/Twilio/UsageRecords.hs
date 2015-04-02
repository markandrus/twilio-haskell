{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageRecords
  ( -- * Resource
    UsageRecords(..)
  , Twilio.UsageRecords.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types
import Twilio.UsageRecord

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

instance Get0 UsageRecords where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Usage/Records.json"

-- | Get 'UsageRecords'.
get :: MonadThrow m => TwilioT m UsageRecords
get = Resource.get
