{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageTriggers
  ( -- * Resource
    UsageTriggers(..)
  , Twilio.UsageTriggers.get
  ) where


import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types
import Twilio.UsageTrigger

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
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Usage/Triggers.json"

-- | Get 'UsageTriggers'.
get :: MonadThrow m => TwilioT m UsageTriggers
get = Resource.get
