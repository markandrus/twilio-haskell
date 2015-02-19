{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.IncomingPhoneNumbers
  ( -- * Resource
    IncomingPhoneNumbers(..)
  , get
  , get'
  ) where

import Twilio.Types
import Twilio.IncomingPhoneNumber (IncomingPhoneNumber)

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson

{- Resource -}

data IncomingPhoneNumbers = IncomingPhoneNumbers
  { incomingPhoneNumberList :: [IncomingPhoneNumber]
  } deriving (Show, Eq)

instance List IncomingPhoneNumbers IncomingPhoneNumber where
  getListWrapper = wrap (const IncomingPhoneNumbers)
  getList = incomingPhoneNumberList
  getPlural = Const "incoming_phone_numbers"

instance FromJSON IncomingPhoneNumbers where
  parseJSON = parseJSONToList

-- | Get 'IncomingPhoneNumbers' for a particular country.
get :: (MonadThrow m, MonadIO m)
    => TwilioT m IncomingPhoneNumbers
get = requestForAccount "/IncomingPhoneNumbers.json"

-- | Get an account's 'IncomingPhoneNumbers' for a particular country.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> TwilioT m IncomingPhoneNumbers
get' = flip forAccount get
