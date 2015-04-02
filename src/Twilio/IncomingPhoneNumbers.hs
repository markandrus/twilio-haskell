{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.IncomingPhoneNumbers
  ( -- * Resource
    IncomingPhoneNumbers(..)
  , Twilio.IncomingPhoneNumbers.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson

import Control.Monad.Twilio
import Twilio.IncomingPhoneNumber
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

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

instance Get0 IncomingPhoneNumbers where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/IncomingPhoneNumbers.json"

-- | Get 'IncomingPhoneNumbers' for a particular country.
get :: MonadThrow m => TwilioT m IncomingPhoneNumbers
get = Resource.get
