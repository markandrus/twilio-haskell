{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.OutgoingCallerIDs
  ( -- * Resource
    OutgoingCallerIDs(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.OutgoingCallerID hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

{- Resource -}

data OutgoingCallerIDs = OutgoingCallerIDs
  { outgoingCallerIDsPagingInformation :: !PagingInformation
  , outgoingCallerIDList :: [OutgoingCallerID]
  } deriving (Show, Eq)

instance List OutgoingCallerIDs OutgoingCallerID where
  getListWrapper = wrap (OutgoingCallerIDs . fromJust)
  getList = outgoingCallerIDList
  getPlural = Const "outgoing_caller_ids"

instance FromJSON OutgoingCallerIDs where
  parseJSON = parseJSONToList

-- | Get 'OutgoingCallerIDs'.
get :: (MonadThrow m, MonadIO m) => TwilioT m OutgoingCallerIDs
get = requestForAccount "/OutgoingCallerIds.json"

-- | Get an account's 'OutgoingCallerIDs'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m OutgoingCallerIDs
get' = flip forAccount get
