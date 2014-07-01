{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.PhoneNumber
  ( -- * Resource
    PhoneNumber(..)
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

{- Resource -}

data PhoneNumber = PhoneNumber
  { sid          :: !PhoneNumberSID
  , friendlyName :: !String
  , phoneNumber  :: !String
  , lata         :: !Integer
  , rateCenter   :: !String
  , latitude     :: !(Maybe Double)
  , longitude    :: !(Maybe Double)
  , region       :: !String
  , postalCode   :: !(Maybe Integer)
  , isoCountry   :: !String
  } deriving (Eq, Show)

instance FromJSON PhoneNumber where
  parseJSON (Object v) = PhoneNumber
    <$>  v .: "sid"
    <*>  v .: "friendly_name"
    <*>  v .: "phone_number"
    <*> (v .: "lata"         >>= safeRead)
    <*>  v .: "rate_center"
    <*> (v .: "latitude"     <&> (=<<) safeRead
                             >>= maybeReturn')
    <*> (v .: "longitude"    <&> (=<<) safeRead
                             >>= maybeReturn')
    <*>  v .: "region"
    <*> (v .: "postal_code"  <&> (=<<) safeRead
                             >>= maybeReturn')
    <*>  v .: "iso_country"
  parseJSON _ = mzero

-- | Get a 'PhoneNumber' by 'PhoneNumberSID'.
get :: (MonadThrow m, MonadIO m) => PhoneNumberSID -> TwilioT m PhoneNumber
get phoneNumberSID
  = requestForAccount
  $ "/AvailablePhoneNumbers/US/Local/" ++ getSID phoneNumberSID ++ ".json"

-- | Get an account's 'PhoneNumber' by 'PhoneNumberSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> PhoneNumberSID
     -> TwilioT m PhoneNumber
get' accountSID phoneNumberSID = forAccount accountSID $ get phoneNumberSID
