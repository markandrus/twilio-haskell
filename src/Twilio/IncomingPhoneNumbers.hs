{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.IncomingPhoneNumbers
  ( -- * Resource
    IncomingPhoneNumbers(..)
  , Twilio.IncomingPhoneNumbers.get
  ) where

import Twilio.Types
import Twilio.IncomingPhoneNumber (IncomingPhoneNumber)

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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
  get0 = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    "/IncomingPhoneNumbers.json"

-- | Get 'IncomingPhoneNumbers' for a particular country.
get :: (MonadThrow m, MonadIO m)
    => TwilioT m IncomingPhoneNumbers
get = Resource.get
