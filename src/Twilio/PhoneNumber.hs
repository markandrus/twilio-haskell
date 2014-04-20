{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.PhoneNumber
  ( PhoneNumberSID
  , PhoneNumber(..)
  , PhoneNumbers
  , phoneNumbers
  ) where

import qualified Twilio.Client as Client
import Twilio.Types

import Control.Applicative ((<$>), (<*>), Const(..))
import Control.Monad (mzero)
import Data.Aeson
import Data.Char (isLower, isNumber)
import Data.Text (unpack)

-- | Phone number 'SID's are 34 characters long and begin with \"PN\".
newtype PhoneNumberSID = PhoneNumberSID { getPhoneNumberSID :: String }
  deriving (Show, Eq)

instance SID PhoneNumberSID where
  getSIDWrapper = wrap PhoneNumberSID
  getPrefix = Const ('P', 'N')
  getSID = getPhoneNumberSID

instance FromJSON PhoneNumberSID where
  parseJSON = parseJSONToSID

data PhoneNumber = PhoneNumber
  { friendlyName :: !String
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
    <$>  v .: "friendly_name"
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

phoneNumbers :: Client.Client -> IO PhoneNumbers
phoneNumbers client = Client.runRequest client "/AvailablePhoneNumbers/US/Local.json" 

data PhoneNumbers = PhoneNumbers
  { phoneNumberList :: [PhoneNumber]
  } deriving (Show, Eq)

instance List PhoneNumbers PhoneNumber where
  getListWrapper = wrap (const PhoneNumbers)
  getList = phoneNumberList
  getPlural = Const "available_phone_numbers"

instance FromJSON PhoneNumbers where
  parseJSON = parseJSONToList
