{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Messages
  ( -- * Resource
    Messages(..)
  , PostMessage(..)
  , get
  , get'
  , post
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))
import Twilio.Message hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)

{- Resource -}

data Messages = Messages
  { messagesPagingInformation :: PagingInformation
  , messageList :: [Message]
  } deriving (Show, Eq)

data PostMessage = PostMessage
  { sendTo   :: !String
  , sendFrom :: !String
  , sendBody :: !String
  } deriving (Show, Eq)

instance List Messages Message where
  getListWrapper = wrap (Messages . fromJust)
  getList = messageList
  getPlural = Const "messages"

instance FromJSON Messages where
  parseJSON = parseJSONToList

-- | Get 'Messages'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Messages
get = requestForAccount "/Messages.json"

-- | Get an account's 'Messages'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m Messages
get' = flip forAccount get

-- | Send a text message.
post :: (MonadThrow m, MonadIO m) => PostMessage -> TwilioT m Message
post msg = postForAccount "/Messages.json"
  [ ("To", encodeUtf8 . pack . sendTo $ msg)
  , ("From", encodeUtf8 . pack . sendFrom $ msg)
  , ("Body", encodeUtf8 . pack . sendBody $ msg)
  ]
