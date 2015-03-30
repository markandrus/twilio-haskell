{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.AvailablePhoneNumbers
  ( -- * Resource
    AvailablePhoneNumbers(..)
  , Twilio.AvailablePhoneNumbers.get
  ) where

import Twilio.Types
import Twilio.AvailablePhoneNumber

import Control.Applicative (Const(Const))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (fromJust)

import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource

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

instance Get1 ISOCountryCode AvailablePhoneNumbers where
  get1 (show -> isoCountryCode) = request (fromJust . parseJSONFromResponse) =<< makeTwilioRequest
    ("/AvailablePhoneNumbers/" ++ isoCountryCode ++ "/Local.json")

-- | Get 'AvailablePhoneNumbers' for a particular country.
get :: (MonadThrow m, MonadIO m)
    => ISOCountryCode
    -> TwilioT m AvailablePhoneNumbers
get = Resource.get
