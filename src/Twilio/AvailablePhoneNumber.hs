{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.AvailablePhoneNumber
  ( -- * Resource
    AvailablePhoneNumber(..)
  ) where

import Control.Applicative
import Control.Error.Safe
import Control.Monad
import Data.Aeson

import Twilio.Types
import Twilio.Internal.Parser

{- Resource -}

data AvailablePhoneNumber = AvailablePhoneNumber
  { friendlyName :: !String
  , phoneNumber  :: !String
  , lata         :: !(Maybe Integer)
  , rateCenter   :: !(Maybe String)
  , latitude     :: !(Maybe Double)
  , longitude    :: !(Maybe Double)
  , region       :: !String
  , postalCode   :: !(Maybe Integer)
  , isoCountry   :: !ISOCountryCode
  , addressRequirements :: !(Maybe AddressRequirement)
  , capabilities        :: !Capabilities
  } deriving (Eq, Show)

instance FromJSON AvailablePhoneNumber where
  parseJSON (Object v) = AvailablePhoneNumber
    <$>  v .: "friendly_name"
    <*>  v .: "phone_number"
    <*> (v .: "lata"         <&> (=<<) readZ
                             >>= maybeReturn')
    <*>  v .: "rate_center"
    <*> (v .: "latitude"     <&> (=<<) readZ
                             >>= maybeReturn')
    <*> (v .: "longitude"    <&> (=<<) readZ
                             >>= maybeReturn')
    <*>  v .: "region"
    <*> (v .: "postal_code"  <&> (=<<) readZ
                             >>= maybeReturn')
    <*>  v .: "iso_country"
    <*>  v .: "address_requirements"
    <*> (v .: "capabilities" >>= parseJSON)
  parseJSON _ = mzero
