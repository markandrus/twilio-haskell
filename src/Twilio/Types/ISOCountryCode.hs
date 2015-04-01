module Twilio.Types.ISOCountryCode where

import Control.Error.Safe
import Control.Monad
import Data.Aeson
import qualified Data.Text as T

-- | Country codes in ISO 3166-1 alpha-2 format supported by Twilio.
data ISOCountryCode
  = AU  -- ^ Australia
  | AT  -- ^ Austria
  | BH  -- ^ Bahrain
  | BE  -- ^ Belgium
  | BR  -- ^ Brazil
  | BG  -- ^ Bulgaria
  | CA  -- ^ Canada
  | CY  -- ^ Cyprus
  | CZ  -- ^ Czech Republic
  | DK  -- ^ Denmark
  | DO  -- ^ Dominican Republic
  | SV  -- ^ El Salvador
  | EE  -- ^ Estonia
  | FI  -- ^ Finland
  | FR  -- ^ France
  | GR  -- ^ Greece
  | HK  -- ^ Hong Kong
  | IE  -- ^ Ireland
  | IL  -- ^ Israel
  | IT  -- ^ Italy
  | JP  -- ^ Japan
  | LV  -- ^ Latvia
  | LT  -- ^ Lithuania
  | LU  -- ^ Luxembourg
  | MT  -- ^ Malta
  | MX  -- ^ Mexico
  | NL  -- ^ The Netherlands
  | NO  -- ^ Norway
  | NZ  -- ^ New Zealand
  | PE  -- ^ Peru
  | PL  -- ^ Poland
  | PT  -- ^ Portugal
  | PR  -- ^ Puerto Rico
  | RO  -- ^ Romania
  | SK  -- ^ Slovakia
  | ZA  -- ^ South Africa
  | ES  -- ^ Spain
  | SE  -- ^ Sweden
  | CH  -- ^ Switzerland
  | GB  -- ^ United Kingdom
  | US  -- ^ United States
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON ISOCountryCode where
  parseJSON (String v) = readZ $ T.unpack v
  parseJSON _ = mzero
