{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageRecords
  ( -- * Resource
    UsageRecords(..)
  , Twilio.UsageRecords.get
  ) where

import Twilio.Types
import Twilio.UsageRecord

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/Usage/Records.json"

-- | Get 'UsageRecords'.
get :: (MonadThrow m, MonadIO m) => TwilioT m UsageRecords
get = Resource.get
