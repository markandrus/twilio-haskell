{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Messages
  ( -- * Resource
    Messages(..)
  , Twilio.Messages.get
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))
import Twilio.Message hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data Messages = Messages
  { messagesPagingInformation :: PagingInformation
  , messageList :: [Message]
  } deriving (Show, Eq)

instance List Messages Message where
  getListWrapper = wrap (Messages . fromJust)
  getList = messageList
  getPlural = Const "messages"

instance FromJSON Messages where
  parseJSON = parseJSONToList

instance Get0 Messages where
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/Messages.json"

-- | Get 'Messages'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Messages
get = Resource.get
