{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Transcriptions
  ( -- * Resource
    Transcriptions(..)
  , Twilio.Transcriptions.get
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))
import Twilio.Transcription hiding (get)

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}
data Transcriptions = Transcriptions
  { transcriptionsPagingInformation :: PagingInformation
  , transcriptionList :: [Transcription]
  } deriving (Show, Eq)

instance List Transcriptions Transcription where
  getListWrapper = wrap (Transcriptions . fromJust)
  getList = transcriptionList
  getPlural = Const "transcriptions"

instance FromJSON Transcriptions where
  parseJSON = parseJSONToList

instance Get0 Transcriptions where
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/Transcriptions.json"

-- | Get 'Transcriptions'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Transcriptions
get = Resource.get
