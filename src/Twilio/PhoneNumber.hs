module Twilio.PhoneNumber
  ( PhoneNumberSID
  ) where

import Twilio.Types

import Control.Monad (mzero)
import Control.Applicative (Const(..))
import Data.Aeson
import Data.Char (isLower, isNumber)
import Data.Text (unpack)

-- | Phone number 'SID's are 34 characters long and begin with \"PN\".
newtype PhoneNumberSID = PhoneNumberSID { getPhoneNumberSID :: String }
  deriving (Show, Eq)

instance SID PhoneNumberSID where
  getSIDWrapper = wrap PhoneNumberSID
  getPrefix = Const ('P', 'N')
  getSID = getPhoneNumberSID

instance FromJSON PhoneNumberSID where
  parseJSON = parseJSONToSID
