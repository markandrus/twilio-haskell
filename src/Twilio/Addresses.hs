{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Addresses
  ( -- * Resource
    Addresses(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.Address hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

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

-- | Get 'Addresses'.
get :: (MonadThrow m, MonadIO m) => TwilioT m Addresses
get = requestForAccount "/Addresses.json"

-- | Get an account's 'Addresses'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m Addresses
get' = flip forAccount get
