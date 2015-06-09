{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Queue.Members
  ( -- * Resource
    Members(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Data
import Data.Maybe
import Data.Monoid
import GHC.Generics

import Twilio.Queue.Member
import Twilio.Internal.Request
import Twilio.Internal.Resource
import Twilio.Types

data Members = Members
  { membersPagingInformation :: PagingInformation
  , memberList :: [Member]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance List Members Member where
  getListWrapper = wrap (Members . fromJust)
  getList = memberList
  getPlural = Const "queue_members"

instance FromJSON Members where
  parseJSON = parseJSONToList

instance Get1 QueueSID Members where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Queues/" <> sid <> ".json")
