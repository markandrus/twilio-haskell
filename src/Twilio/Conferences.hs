{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Conferences
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Conferences
  ( -- * Resource
    Conferences(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Data
import Data.Maybe
import GHC.Generics

import Twilio.Conference
import Twilio.Internal.Request
import Twilio.Internal.Resource
import Twilio.Types

data Conferences = Conferences
  { conferencesPagingInformation :: PagingInformation
  , conferenceList :: [Conference]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance List Conferences Conference where
  getListWrapper = wrap (Conferences . fromJust)
  getList = conferenceList
  getPlural = Const "conferences"

instance FromJSON Conferences where
  parseJSON = parseJSONToList

instance Get0 Conferences where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Conferences.json"
