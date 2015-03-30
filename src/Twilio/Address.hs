{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.Address
  ( -- * Resource
    Address(..)
  , Twilio.Address.get
  ) where

import Twilio.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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

instance Get1 AddressSID Address where
  get1 (getSID -> sid) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    ("/Addresses" ++ sid ++ ".json")

-- | Get an 'Address' by 'AddressSID'.
get :: Monad m => AddressSID -> TwilioT m Address
get = Resource.get
