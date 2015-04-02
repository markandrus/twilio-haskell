{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Transcriptions
  ( -- * Resource
    Transcriptions(..)
  , Twilio.Transcriptions.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Transcription
import Twilio.Types

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
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Transcriptions.json"

-- | Get 'Transcriptions'.
get :: MonadThrow m => TwilioT m Transcriptions
get = Resource.get
