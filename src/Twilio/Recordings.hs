{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Recordings
  ( -- * Resource
    Recordings(..)
  , Twilio.Recordings.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Recording
import Twilio.Types

{- Resource -}
data Recordings = Recordings
  { recordingsPagingInformation :: PagingInformation
  , recordingList :: [Recording]
  } deriving (Show, Eq)

instance List Recordings Recording where
  getListWrapper = wrap (Recordings . fromJust)
  getList = recordingList
  getPlural = Const "recordings"

instance FromJSON Recordings where
  parseJSON = parseJSONToList

instance Get0 Recordings where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Recordings.json"

-- | Get 'Recordings'.
get :: MonadThrow m => TwilioT m Recordings
get = Resource.get
