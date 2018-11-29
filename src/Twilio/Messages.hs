{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Messages
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Messages
  ( -- * Resource
    Messages(..)
  , PostMessage(..)
  , PostCopilotMessage(..)
  , Twilio.Messages.get
  , Twilio.Messages.post
  , Twilio.Messages.postCopilot
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding
import Data.Maybe
import qualified Data.ByteString.Char8 as C

import Control.Monad.Twilio
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Message
import Twilio.Types
import Twilio.Types.SID (sidToText)

{- Resource -}

data Messages = Messages
  { messagesPagingInformation :: PagingInformation
  , messageList :: [Message]
  } deriving (Show, Eq)

data PostMessage = PostMessage
  { sendTo   :: !Text
  , sendFrom :: !Text
  , sendBody :: !Text
  , statusCallback :: !(Maybe Text)
  } deriving (Show, Eq)

data PostCopilotMessage = PostCopilotMessage
  { coSendTo :: !Text
  , coMsgId :: !MessagingServiceSID
  , coBody :: !Text
  } deriving (Show, Eq)

instance Post1 PostCopilotMessage Message where
  post1 msg =
    request parseJSONFromResponse =<<
    makeTwilioPOSTRequest
      "/Messages.json"
      [ ("To", encodeUtf8 $ coSendTo msg)
      , ("Body", encodeUtf8 $ coBody msg)
      , ( "MessagingServiceSid"
        , encodeUtf8 . sidToText . getMessagingServiceSID $ coMsgId msg)
      ]

instance List Messages Message where
  getListWrapper = wrap (Messages . fromJust)
  getList = messageList
  getPlural = Const "messages"

instance FromJSON Messages where
  parseJSON = parseJSONToList

instance Get0 Messages where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Messages.json"

instance Post1 PostMessage Message where
  post1 msg = request parseJSONFromResponse =<<
    makeTwilioPOSTRequest "/Messages.json" (requiredParams ++ optionalParams)
    where requiredParams = [ ("To",   encodeUtf8 $ sendTo msg)
                           , ("From", encodeUtf8 $ sendFrom msg)
                           , ("Body", encodeUtf8 $ sendBody msg)
                           ]
          optionalParams = callBack
          callBack       = toSingleParam "StatusCallback" (statusCallback msg)

type ParamName  = C.ByteString
type ParamValue = Text
type ParamPair  = (C.ByteString, C.ByteString)

toSingleParam :: ParamName -> Maybe ParamValue -> [ParamPair]
toSingleParam name mVal = maybe mempty toParamPair mVal
    where toParamPair val = [(name, encodeUtf8 val)]

-- | Get 'Messages'.
get :: MonadThrow m => TwilioT m Messages
get = Resource.get

-- | Send a text message.
post :: MonadThrow m => PostMessage -> TwilioT m Message
post = Resource.post

-- | Send a text message using Copilot
postCopilot
  :: MonadThrow m
  => PostCopilotMessage -> TwilioT m Message
postCopilot = Resource.post
