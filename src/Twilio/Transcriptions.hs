{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Transcriptions
  ( -- * Resource
    Transcriptions(..)
  , get
  , get'
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))
import Twilio.Transcription hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

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

-- | Get 'Transcriptions'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Transcriptions
get = requestForAccount "/Transcriptions.json"

-- | Get an account's 'Transcriptions'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m Transcriptions
get' = flip forAccount get
