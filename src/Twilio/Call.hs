{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Call
  ( -- * Resource
    Call(..)
  , CallSID
  , get
  , get'
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

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
  , direction      :: !CallDirection
  , answeredBy     :: !(Maybe AnsweredBy)
  , forwardedFrom  :: !(Maybe String)
  , callerName     :: !(Maybe String)
  , uri            :: !URI
  , apiVersion     :: !APIVersion
  } deriving (Show, Eq)

maybeParseSID str = case parseSID str of
  Left  _   -> Nothing
  Right sid -> Just sid

instance FromJSON Call where
  parseJSON (Object v) = Call
    <$>  v .: "sid"
    <*>  v .: "parent_call_sid"
    <*> (v .: "date_created"     >>= parseDateTime)
    <*> (v .: "date_updated"     >>= parseDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "to"               <&> filterEmpty
    <*>  v .: "from"
    <*> (v .: "phone_number_sid" <&> filterEmpty
                                 <&> (=<<) maybeParseSID)
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
    <*>  v .: "forwarded_from"   <&> filterEmpty
    <*>  v .: "caller_name"
    <*> (v .: "uri"              <&> parseRelativeReference
                                 >>= maybeReturn)
    <*>  v .: "api_version"
  parseJSON _ = mzero

-- | Get a 'Call' by 'CallSID'.
get :: (MonadThrow m, MonadIO m) => CallSID -> TwilioT m Call
get callSID = requestForAccount $ "/Calls/" ++ getSID callSID ++ ".json"

-- | Get an account's 'Call' by 'CallSID'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> CallSID -> TwilioT m Call
get' accountSID callSID = forAccount accountSID $ get callSID
