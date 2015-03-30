{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Call
  ( -- * Resource
    Call(..)
  , CallSID
  , Twilio.Call.get
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

{- Resource -}

data Call = Call
  { sid            :: !CallSID
  , parentCallSID  :: !(Maybe CallSID)
  , dateCreated    :: !UTCTime
  , dateUpdated    :: !UTCTime
  , accountSID     :: !AccountSID
  , to             :: !(Maybe String)
  , from           :: !String
  , phoneNumberSID :: !(Maybe PhoneNumberSID)
  , status         :: !CallStatus
  , startTime      :: !UTCTime
  , endTime        :: !(Maybe UTCTime)
  , duration       :: !(Maybe Int)
  , price          :: !(Maybe Double)
  , priceUnit      :: !(Maybe PriceUnit)
  , direction      :: !(Maybe CallDirection)
  , answeredBy     :: !(Maybe AnsweredBy)
  , forwardedFrom  :: !(Maybe String)
  , callerName     :: !(Maybe String)
  , uri            :: !URI
  , apiVersion     :: !APIVersion
  } deriving (Show, Eq)

instance FromJSON Call where
  parseJSON (Object v) = Call
    <$>  v .: "sid"
    <*>  v .: "parent_call_sid"
    <*> (v .: "date_created"     >>= parseDateTime)
    <*> (v .: "date_updated"     >>= parseDateTime)
    <*>  v .: "account_sid"
    <*> (v .: "to"               <&> filterEmpty)
    <*>  v .: "from"
    <*> (v .: "phone_number_sid" <&> (=<<) filterEmpty
                                 <&> (=<<) parseSID)
    <*>  v .: "status"
    <*> (v .: "start_time"       >>= parseDateTime)
    <*> (v .: "end_time"         <&> (=<<) parseDateTime)
    <*> (v .: "duration"         <&> fmap safeRead
                                 >>= maybeReturn')
    <*> (v .: "price"            <&> fmap safeRead
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
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    ("/Calls/" ++ sid ++ ".json")

-- | Get a 'Call' by 'CallSID'.
get :: (MonadThrow m, MonadIO m) => CallSID -> TwilioT m Call
get = Resource.get
