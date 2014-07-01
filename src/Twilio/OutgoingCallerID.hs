{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.OutgoingCallerID
  ( -- * Resource
    OutgoingCallerID(..)
  , PhoneNumberSID
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

data OutgoingCallerID = OutgoingCallerID
  { sid          :: !PhoneNumberSID
  , dateCreated  :: !UTCTime
  , dateUpdated  :: !UTCTime
  , friendlyName :: !String
  , accountSID   :: !AccountSID
  , phoneNumber  :: !String
  , uri          :: !URI
  } deriving (Show, Eq)

instance FromJSON OutgoingCallerID where
  parseJSON (Object v) = OutgoingCallerID
    <$>  v .: "sid"
    <*> (v .: "date_created"  >>= parseDateTime)
    <*> (v .: "date_updated"  >>= parseDateTime)
    <*>  v .: "friendly_name"
    <*>  v .: "account_sid"
    <*>  v .: "phone_number"
    <*> (v .: "uri"           <&> parseRelativeReference
                              >>= maybeReturn)
  parseJSON _ = mzero

-- | Get an 'OutgoingCallerID' by 'PhoneNumberSID'
get :: (MonadThrow m, MonadIO m)
    => PhoneNumberSID
    -> TwilioT m OutgoingCallerID
get phoneNumberSID
  = requestForAccount
  $ "/OutgoingCallerIds/" ++ getSID phoneNumberSID ++ ".json"

-- | Get an account's 'OutgoingCallerID' by 'PhoneNumberSID'
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> PhoneNumberSID
     -> TwilioT m OutgoingCallerID
get' accountSID phoneNumberSID = forAccount accountSID $ get phoneNumberSID
