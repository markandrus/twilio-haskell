module Twilio.Account
  ( AccountSID
  ) where

import Twilio.Types

import Control.Monad (mzero)
import Control.Applicative (Const(..))
import Data.Aeson
import Data.Char (isLower, isNumber)
import Data.Text (unpack)

-- | Account 'SID's are 34 characters long and begin with \"AC\".
newtype AccountSID = AccountSID { getAccountSID :: String }
  deriving (Show, Eq)

instance SID AccountSID where
  getSIDWrapper = wrap AccountSID
  getPrefix = Const ('A', 'C')
  getSID = getAccountSID

instance FromJSON AccountSID where
  parseJSON = parseJSONToSID
