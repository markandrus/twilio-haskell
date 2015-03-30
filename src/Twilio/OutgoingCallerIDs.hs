{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.OutgoingCallerIDs
  ( -- * Resource
    OutgoingCallerIDs(..)
  , Twilio.OutgoingCallerIDs.get
  ) where

import Twilio.Types
import Twilio.OutgoingCallerID hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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

instance Get0 OutgoingCallerIDs where
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/OutgoingCallerIds.json"

-- | Get 'OutgoingCallerIDs'.
get :: (MonadThrow m, MonadIO m) => TwilioT m OutgoingCallerIDs
get = Resource.get
