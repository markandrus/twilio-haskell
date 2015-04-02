{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Address
  ( -- * Resource
    Address(..)
  , Twilio.Address.get
  ) where

import Control.Applicative
import Control.Error.Safe
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import Data.Text (Text)

import Control.Monad.Twilio
import Twilio.Internal.Parser
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Address = Address
  { sid          :: !AddressSID
  , accountSID   :: !AccountSID
  , friendlyName :: !Text
  , customerName :: !Text
  , street       :: !Text
  , city         :: !Text
  , region       :: !Text
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
    <*> (v .: "postal_code"  <&> (=<<) readZ
                             >>= maybeReturn')
    <*>  v .: "iso_country"
  parseJSON _ = mzero

instance Get1 AddressSID Address where
  get1 (getSID -> sid) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/Addresses" <> sid <> ".json")

-- | Get an 'Address' by 'AddressSID'.
get :: MonadThrow m => AddressSID -> TwilioT m Address
get = Resource.get
