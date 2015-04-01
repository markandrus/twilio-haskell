{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.OutgoingCallerIDs
  ( -- * Resource
    OutgoingCallerIDs(..)
  , Twilio.OutgoingCallerIDs.get
  ) where


import Control.Applicative
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.OutgoingCallerID
import Twilio.Types

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
get :: Monad m => TwilioT m OutgoingCallerIDs
get = Resource.get
