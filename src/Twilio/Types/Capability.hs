{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeSynonymInstances #-}

module Twilio.Types.Capability where

import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

type Capabilities = Set Capability

data Capability
  = Voice
  | SMS
  | MMS
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance {-# OVERLAPPING #-} FromJSON Capabilities where
  parseJSON (Object map) 
    = let map' = fmap (\value -> case value of
                        Bool bool     -> bool
                        _             -> False) map
      in  return $ foldr (\capability set ->
            if HashMap.lookupDefault False (T.pack $ show capability) map'
              then Set.insert capability set
              else set
          ) Set.empty [Voice, SMS, MMS]
  parseJSON _ = mzero
