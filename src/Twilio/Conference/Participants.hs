{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Conference.Participants
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Conference.Participants
  ( -- * Resource
    Participants(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Data
import Data.Maybe
import GHC.Generics

import Twilio.Conference.Participant
import Twilio.Internal.Request
import Twilio.Internal.Resource
import Twilio.Types

data Participants = Participants
  { participantsPagingInformation :: PagingInformation
  , participantList :: [Participant]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance List Participants Participant where
  getListWrapper = wrap (Participants . fromJust)
  getList = participantList
  getPlural = Const "participants"

instance FromJSON Participants where
  parseJSON = parseJSONToList

instance Get1 ConferenceSID Participants where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Conferences/" <> sid <> ".json")
