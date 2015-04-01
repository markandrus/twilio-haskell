{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Messages
  ( -- * Resource
    Messages(..)
  , Twilio.Messages.get
  ) where

import Control.Applicative
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Message
import Twilio.Types

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
get :: Monad m => TwilioT m Messages
get = Resource.get
