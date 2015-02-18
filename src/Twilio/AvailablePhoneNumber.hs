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
  parseJSON _ = mzero

data AddressRequirement
  = None
  | Any
  | Local
  | Foreign
  deriving (Bounded, Enum, Eq, Ord)

instance Show AddressRequirement where
  show None    = "none"
  show Any     = "any"
  show Local   = "local"
  show Foreign = "foreign"

instance FromJSON AddressRequirement where
  parseJSON (String "none")    = return None
  parseJSON (String "any")     = return Any
  parseJSON (String "local")   = return Local
  parseJSON (String "foreign") = return Foreign
  parseJSON _ = mzero
