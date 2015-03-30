{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageTriggers
  ( -- * Resource
    UsageTriggers(..)
  , Twilio.UsageTriggers.get
  ) where

import Twilio.Types
import Twilio.UsageTrigger hiding (get)

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data UsageTriggers = UsageTriggers
  { usageTriggersPagingInformation :: PagingInformation
  , usageTriggerList :: [UsageTrigger]
  } deriving (Show, Eq)

instance List UsageTriggers UsageTrigger where
  getListWrapper = wrap (UsageTriggers . fromJust)
  getList = usageTriggerList
  getPlural = Const "usage_triggers"

instance FromJSON UsageTriggers where
  parseJSON = parseJSONToList

instance Get0 UsageTriggers where
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/Usage/Triggers.json"

-- | Get 'UsageTriggers'.
get :: (MonadThrow m, MonadIO m) => TwilioT m UsageTriggers
get = Resource.get
