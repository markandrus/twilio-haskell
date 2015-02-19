{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Recordings
  ( -- * Resource
    Recordings(..)
  , get
  , get'
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))
import Twilio.Recording hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

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

-- | Get 'Recordings'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Recordings
get = requestForAccount "/Recordings.json"

-- | Get an account's 'Recordings'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m Recordings
get' = flip forAccount get
