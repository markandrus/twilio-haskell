{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Addresses
  ( -- * Resource
    Addresses(..)
  , Twilio.Addresses.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe

import Control.Monad.Twilio
import Twilio.Address
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Addresses = Addresses
  { addressesPagingInformation :: PagingInformation
  , addressList :: [Address]
  } deriving (Show, Eq)

instance List Addresses Address where
  getListWrapper = wrap (Addresses . fromJust)
  getList = addressList
  getPlural = Const "addresses"

instance FromJSON Addresses where
  parseJSON = parseJSONToList

instance Get0 Addresses where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest "/Addresses.json"

get :: MonadThrow m => TwilioT m Addresses
get = Resource.get
