{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Queues
  ( -- * Resource
    Queues(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Data
import Data.Maybe
import GHC.Generics

import Twilio.Queue
import Twilio.Internal.Request
import Twilio.Internal.Resource
import Twilio.Types

data Queues = Queues
  { queuesPagingInformation :: PagingInformation
  , queueList :: [Queue]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance List Queues Queue where
  getListWrapper = wrap (Queues . fromJust)
  getList = queueList
  getPlural = Const "queues"

instance FromJSON Queues where
  parseJSON = parseJSONToList

instance Get0 Queues where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Queues.json"
