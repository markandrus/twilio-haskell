{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Call
  ( -- * Resource
    Call(..)
  , CallSID
  , Twilio.Call.get
    -- * Types
  , AnsweredBy(..)
  , CallDirection(..)
  , CallStatus(..)
  ) where

import Control.Applicative
import Control.Error.Safe
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

data Call = Call
  { sid            :: !CallSID
  , parentCallSID  :: !(Maybe CallSID)
  , dateCreated    :: !(Maybe UTCTime) -- "date_created" is initially null
  , dateUpdated    :: !(Maybe UTCTime) -- "date_updated" is initially null
  , accountSID     :: !AccountSID
  , to             :: !(Maybe Text)
  , from           :: !Text
  , phoneNumberSID :: !(Maybe PhoneNumberSID)
  , status         :: !CallStatus
  , startTime      :: !(Maybe UTCTime) -- "start_time" is initially null
  , endTime        :: !(Maybe UTCTime)
  , duration       :: !(Maybe Int)
  , price          :: !(Maybe Double)
  , priceUnit      :: !(Maybe PriceUnit)
  , direction      :: !(Maybe CallDirection)
  , answeredBy     :: !(Maybe AnsweredBy)
  , forwardedFrom  :: !(Maybe Text)
  , callerName     :: !(Maybe Text)
  , uri            :: !URI
  , apiVersion     :: !APIVersion
  } deriving (Show, Eq)

instance FromJSON Call where
  parseJSON (Object v) = Call
    <$>  v .: "sid"
    <*>  v .: "parent_call_sid"
    <*> (v .: "date_created"     <&> (=<<) parseDateTime)
    <*> (v .: "date_updated"     <&> (=<<) parseDateTime)
    <*>  v .: "account_sid"
    <*> (v .: "to"               <&> filterEmpty)
    <*>  v .: "from"
    <*> (v .: "phone_number_sid" <&> (=<<) filterEmpty
                                 <&> (=<<) parseSID)
    <*>  v .: "status"
    <*> (v .: "start_time"       <&> (=<<) parseDateTime)
    <*> (v .: "end_time"         <&> (=<<) parseDateTime)
    <*> (v .: "duration"         <&> fmap readZ
                                 >>= maybeReturn')
    <*> (v .: "price"            <&> fmap readZ
                                 >>= maybeReturn')
    <*>  v .: "price_unit"
    <*>  v .: "direction"
    <*>  v .: "answered_by"
    <*>  v .: "forwarded_from"   <&> (=<<) filterEmpty
    <*>  v .: "caller_name"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
    <*>  v .: "api_version"
  parseJSON _ = mzero

instance Get1 CallSID Call where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Calls/" <> sid <> ".json")

-- | Get a 'Call' by 'CallSID'.
get :: MonadThrow m => CallSID -> TwilioT m Call
get = Resource.get

{- Types -}

data AnsweredBy
  = Human
  | Machine
  deriving Eq

instance Show AnsweredBy where
  show Human   = "human"
  show Machine = "machine"

instance FromJSON AnsweredBy where
  parseJSON (String "human")   = return Human
  parseJSON (String "machine") = return Machine
  parseJSON _ = mzero

data CallDirection
  = Inbound
  | OutboundAPI
  | OutboundDial
  deriving Eq

instance Show CallDirection where
  show Inbound      = "inbound"
  show OutboundAPI  = "outbound-api"
  show OutboundDial = "outbound-dial"

instance FromJSON CallDirection where
  parseJSON (String "inbound")       = return Inbound
  parseJSON (String "outbound-api")  = return OutboundAPI
  parseJSON (String "outbound-dial") = return OutboundDial
  parseJSON _ = mzero

data CallStatus
  = Queued
  | Ringing
  | InProgress
  | Canceled
  | Completed
  | Failed
  | Busy
  | NoAnswer
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON CallStatus where
  parseJSON (String "queued")      = return Queued
  parseJSON (String "ringing")     = return Ringing
  parseJSON (String "in-progress") = return InProgress
  parseJSON (String "canceled")    = return Canceled
  parseJSON (String "completed")   = return Completed
  parseJSON (String "failed")      = return Failed
  parseJSON (String "busy")        = return Busy
  parseJSON (String "no-answer")   = return NoAnswer
  parseJSON _ = mzero
