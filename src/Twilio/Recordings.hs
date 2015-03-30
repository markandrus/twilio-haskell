{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Recordings
  ( -- * Resource
    Recordings(..)
  , Twilio.Recordings.get
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))
import Twilio.Recording hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/Recordings.json"

-- | Get 'Recordings'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Recordings
get = Resource.get
