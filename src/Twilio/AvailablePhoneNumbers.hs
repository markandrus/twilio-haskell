{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.AvailablePhoneNumbers
  ( -- * Resource
    AvailablePhoneNumbers(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.AvailablePhoneNumber

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson

{- Resource -}

data AvailablePhoneNumbers = AvailablePhoneNumbers
  { availablePhoneNumberList :: [AvailablePhoneNumber]
  } deriving (Show, Eq)

instance List AvailablePhoneNumbers AvailablePhoneNumber where
  getListWrapper = wrap (const AvailablePhoneNumbers)
  getList = availablePhoneNumberList
  getPlural = Const "available_phone_numbers"

instance FromJSON AvailablePhoneNumbers where
  parseJSON = parseJSONToList

-- | Get 'AvailablePhoneNumbers' for a particular country.
get :: (MonadThrow m, MonadIO m)
    => ISOCountryCode
    -> TwilioT m AvailablePhoneNumbers
get isoCountryCode
  = requestForAccount
  $ "/AvailablePhoneNumbers/" ++ show isoCountryCode ++ "/Local.json"

-- | Get an account's 'AvailablePhoneNumbers' for a particular country.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> ISOCountryCode
     -> TwilioT m AvailablePhoneNumbers
get' accountSid isoCountryCode = flip forAccount (get isoCountryCode) accountSid
