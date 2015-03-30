{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Addresses
  ( -- * Resource
    Addresses(..)
  , Twilio.Addresses.get
  ) where

import Twilio.Types
import Twilio.Address

import Control.Applicative
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest "/Addresses.json"

get :: Monad m => TwilioT m Addresses
get = Resource.get
