{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ViewPatterns #-}

module Twilio.AvailablePhoneNumbers
  ( -- * Resource
    AvailablePhoneNumbers(..)
  , Twilio.AvailablePhoneNumbers.get
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Monoid
import qualified Data.Text as T

import Control.Monad.Twilio
import Twilio.AvailablePhoneNumber
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

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
  get1 (show -> isoCountryCode) = request parseJSONFromResponse =<< makeTwilioRequest
    ("/AvailablePhoneNumbers/" <> T.pack isoCountryCode <> "/Local.json")

-- | Get 'AvailablePhoneNumbers' for a particular country.
get :: MonadThrow m => ISOCountryCode -> TwilioT m AvailablePhoneNumbers
get = Resource.get
