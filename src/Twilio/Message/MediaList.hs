{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Message.MediaList
  ( -- * Resource
    MediaList(..)
    -- * Types
  ) where

import Control.Applicative
import Data.Aeson
import Data.Data
import Data.Maybe
import Data.Monoid
import GHC.Generics

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Message.Media
import Twilio.Types

data MediaList = MediaList
  { mediaPagingInformation :: PagingInformation
  , mediaList :: [Media]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance List MediaList Media where
  getListWrapper = wrap (MediaList . fromJust)
  getList = mediaList
  getPlural = Const "media_list"

instance FromJSON MediaList where
  parseJSON = parseJSONToList

instance Get1 MessageSID MediaList where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Messages/" <> sid <> "/Media.json")
