{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Types.AddressRequirement where

import Control.Monad
import Data.Aeson

data AddressRequirement
  = None
  | Any
  | Local
  | Foreign
  deriving (Bounded, Enum, Eq, Ord)

instance Read AddressRequirement where
  readsPrec _ = \case
    "none"    -> return (None, "none")
    "any"     -> return (None, "any")
    "local"   -> return (None, "local")
    "foreign" -> return (None, "foregin")
    _         -> mzero

instance Show AddressRequirement where
  show None    = "none"
  show Any     = "any"
  show Local   = "local"
  show Foreign = "foreign"

instance FromJSON AddressRequirement where
  parseJSON (String "none")    = return None
  parseJSON (String "any")     = return Any
  parseJSON (String "local")   = return Local
  parseJSON (String "foreign") = return Foreign
  parseJSON _ = mzero
