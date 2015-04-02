{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Message
  ( -- * Resource
    Message(..)
  , Twilio.Message.get
    -- * Types
  , MessageDirection(..)
  , MessageStatus(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Network.URI

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Message = Message
  { sid         :: !MessageSID
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  , dateSent    :: !(Maybe UTCTime)
    -- ^ This will be Nothing if Twilio hasn't sent the message yet.
  , accountSID  :: !AccountSID
  , to          :: !Text
  , from        :: !Text
  , body        :: !Text
  , status      :: !MessageStatus
--  , numSegments :: !Integer
  , direction   :: !MessageDirection
--  , price       :: !Double
  , priceUnit   :: !PriceUnit
  , apiVersion  :: !APIVersion
  , uri         :: !URI
  } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*> (v .:? "date_sent" >>= parseMaybeDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "to"
    <*>  v .: "from"
    <*>  v .: "body"
    <*>  v .: "status"
--     <*> (v .: "num_segments" <&> fmap safeRead
--                              >>= maybeReturn)
    <*>  v .: "direction"
--    <*> (v .: "price"        <&> fmap safeRead
--                             >>= maybeReturn')
    <*>  v .: "price_unit"
    <*>  v .: "api_version"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

instance Get1 MessageSID Message where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Messages/" <> sid <> ".json")

-- | Get a 'Message' by 'MessageSID'.
get :: MonadThrow m => MessageSID -> TwilioT m Message
get = Resource.get

{- Types -}

data MessageDirection
  = Inbound
  | OutboundAPI
  | OutboundCall
  | OutboundReply
  deriving Eq

instance Show MessageDirection where
  show Inbound       = "inbound"
  show OutboundAPI   = "outbound-api"
  show OutboundCall  = "outbound-call"
  show OutboundReply = "outbound-reply"

instance FromJSON MessageDirection where
  parseJSON (String "inbound")        = return Inbound
  parseJSON (String "outbound-api")   = return OutboundAPI
  parseJSON (String "outbound-call")  = return OutboundCall
  parseJSON (String "outbound-reply") = return OutboundReply
  parseJSON _ = mzero

data MessageStatus
  = Queued
  | Sending
  | Sent
  | Failed
  | Received
  | Delivered
  | Undelivered
  deriving Eq

instance Show MessageStatus where
  show Queued      = "queued"
  show Sending     = "sending"
  show Sent        = "sent"
  show Failed      = "failed"
  show Received    = "received"
  show Delivered   = "delivered"
  show Undelivered = "undelivered"

instance FromJSON MessageStatus where
  parseJSON (String "queued")      = return Queued
  parseJSON (String "sending")     = return Sending
  parseJSON (String "sent")        = return Sent
  parseJSON (String "failed")      = return Failed
  parseJSON (String "received")    = return Received
  parseJSON (String "delivered")   = return Delivered
  parseJSON (String "undelivered") = return Undelivered
  parseJSON _ = mzero
