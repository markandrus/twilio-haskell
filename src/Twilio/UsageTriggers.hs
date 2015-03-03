{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.UsageTriggers
  ( -- * Resource
    UsageTriggers(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.UsageTrigger hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

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

-- | Get 'UsageTriggers'.
get :: (MonadThrow m, MonadIO m) => TwilioT m UsageTriggers
get = requestForAccount "/Usage/Triggers.json"

-- | Get an account's 'UsageTriggers'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m UsageTriggers
get' = flip forAccount get
