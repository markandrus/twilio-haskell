{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.AvailablePhoneNumber
  ( -- * Resource
    AvailablePhoneNumber(..)
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson

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
    <*> (v .: "lata"         <&> (=<<) safeRead
                             >>= maybeReturn')
    <*>  v .: "rate_center"
    <*> (v .: "latitude"     <&> (=<<) safeRead
                             >>= maybeReturn')
    <*> (v .: "longitude"    <&> (=<<) safeRead
                             >>= maybeReturn')
    <*>  v .: "region"
    <*> (v .: "postal_code"  <&> (=<<) safeRead
                             >>= maybeReturn')
    <*>  v .: "iso_country"
    <*>  v .: "address_requirements"
    <*>  v .: "capabilities" <&> parseCapabilitiesFromJSON
  parseJSON _ = mzero
