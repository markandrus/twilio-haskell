{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}

module Twilio.Queues
  ( -- * Resource
    Queues(..)
  , Twilio.Queues.get
  , PostQueues(..)
  , Twilio.Queues.post
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Data
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import GHC.Generics

import Control.Monad.Twilio
import Twilio.Queue
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

data Queues = Queues
  { queuesPagingInformation :: PagingInformation
  , queueList :: [Queue]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance List Queues Queue where
  getListWrapper = wrap (Queues . fromJust)
  getList = queueList
  getPlural = Const "queues"

instance FromJSON Queues where
  parseJSON = parseJSONToList

instance Get0 Queues where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Queues.json"

get :: MonadThrow m => TwilioT m Queues
get = Resource.get

data PostQueues = PostQueues
  { friendlyName :: !(Maybe Text)
  , maxSize      :: !(Maybe Int)
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Post1 PostQueues Queue where
  post1 postQueues = request parseJSONFromResponse =<<
    makeTwilioPOSTRequest "/Queues.json" (catMaybes
      [ ("FriendlyName",) . encodeUtf8 <$> Twilio.Queues.friendlyName postQueues
      , ("MaxSize",) . encodeUtf8 . pack . show <$> Twilio.Queues.maxSize postQueues
      ] )

post :: MonadThrow m => PostQueues -> TwilioT m Queue
post = Resource.post
