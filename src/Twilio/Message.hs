{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Message
  ( -- * Resource
    Message(..)
  , get
  , get'
    -- * Types
  , MessageDirection(..)
  , MessageStatus(..)
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

{- Resource -}

data Message = Message
  { sid :: !MessageSID
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  , dateSent    :: !UTCTime
  , accountSID  :: !AccountSID
  , from        :: !String
  , to          :: !String
  , body        :: !String
--  , numSegments :: !Integer
  , status      :: !MessageStatus
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
    <*> (v .: "date_sent"    >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "from"
    <*>  v .: "to"
    <*>  v .: "body"
--    <*> (v .: "num_segments" <&> fmap safeRead
--                             >>= maybeReturn')
    <*>  v .: "status"
    <*>  v .: "direction"
--    <*> (v .: "price"        <&> fmap safeRead
--                             >>= maybeReturn')
    <*>  v .: "price_unit"
    <*>  v .: "api_version"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  parseJSON _ = mzero

-- | Get a 'Message' by 'MessageSID'.
get :: (MonadThrow m, MonadIO m) => MessageSID -> TwilioT m Message
get messageSID
  = requestForAccount $ "/Messages/" ++ getSID messageSID ++ ".json"

-- | Get an account's 'Message' by 'MessageSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> MessageSID
     -> TwilioT m Message
get' accountSID messageSID = forAccount accountSID $ get messageSID

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
  deriving Eq

instance Show MessageStatus where
  show Queued   = "queued"
  show Sending  = "sending"
  show Sent     = "sent"
  show Failed   = "failed"
  show Received = "received"

instance FromJSON MessageStatus where
  parseJSON (String "queued")   = return Queued
  parseJSON (String "sending")  = return Sending
  parseJSON (String "sent")     = return Sent
  parseJSON (String "failed")   = return Failed
  parseJSON (String "received") = return Received
  parseJSON _ = mzero
