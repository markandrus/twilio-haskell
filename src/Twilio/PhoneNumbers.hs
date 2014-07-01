{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.PhoneNumbers
  ( -- * Resource
    PhoneNumber(..)
  , PhoneNumberSID
    -- * List Resource
  , PhoneNumbers(..)
  , get
  , get'
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>), Const(..))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson

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

get :: (MonadThrow m, MonadIO m) => TwilioT m PhoneNumbers
get = requestForAccount "/AvailablePhoneNumbers/US/Local.json"

get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m PhoneNumbers
get' = flip forAccount get

data PhoneNumbers = PhoneNumbers
  { phoneNumberList :: [PhoneNumber]
  } deriving (Show, Eq)

instance List PhoneNumbers PhoneNumber where
  getListWrapper = wrap (const PhoneNumbers)
  getList = phoneNumberList
  getPlural = Const "available_phone_numbers"

instance FromJSON PhoneNumbers where
  parseJSON = parseJSONToList
