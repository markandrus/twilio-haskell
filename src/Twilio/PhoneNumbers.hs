{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.PhoneNumbers
  ( -- * Resource
    PhoneNumbers(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.PhoneNumber hiding (get, get')

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson

{- Resource -}

data PhoneNumbers = PhoneNumbers
  { phoneNumberList :: [PhoneNumber]
  } deriving (Show, Eq)

instance List PhoneNumbers PhoneNumber where
  getListWrapper = wrap (const PhoneNumbers)
  getList = phoneNumberList
  getPlural = Const "available_phone_numbers"

instance FromJSON PhoneNumbers where
  parseJSON = parseJSONToList

-- | Get available 'PhoneNumbers'.
get :: (MonadThrow m, MonadIO m) => TwilioT m PhoneNumbers
get = requestForAccount "/AvailablePhoneNumbers/US/Local.json"

-- | Get an account's available 'PhoneNumbers'.
get' :: (MonadThrow m, MonadIO m) => AccountSID -> TwilioT m PhoneNumbers
get' = flip forAccount get
