{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Address
  ( -- * Resource
    Address(..)
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

data Address = Address
  { sid          :: !AddressSID
  , accountSID   :: !AccountSID
  , friendlyName :: !String
  , customerName :: !String
  , street       :: !String
  , city         :: !String
  , region       :: !String
  , postalCode   :: !(Maybe Integer)
  , isoCountry   :: !ISOCountryCode
  } deriving (Eq, Show)

instance FromJSON Address where
  parseJSON (Object v) = Address
    <$>  v .: "sid"
    <*>  v .: "account_sid"
    <*>  v .: "friendly_name"
    <*>  v .: "customer_name"
    <*>  v .: "street"
    <*>  v .: "city"
    <*>  v .: "region"
    <*> (v .: "postal_code"  <&> (=<<) safeRead
                             >>= maybeReturn')
    <*>  v .: "iso_country"
  parseJSON _ = mzero

-- | Get an 'Address' by 'AddressSID'.
get :: (MonadThrow m, MonadIO m) => AddressSID -> TwilioT m Address
get addressSID
  = requestForAccount $ "/Addresses/" ++ getSID addressSID ++ ".json"

-- | Get an 'Address' for an account by 'AddressSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> AddressSID
     -> TwilioT m Address
get' accountSID addressSID = forAccount accountSID $ get addressSID
