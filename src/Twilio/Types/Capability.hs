{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Capability
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Types.Capability where

import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

keyFromString :: String -> Key.Key
keyFromString = Key.fromString

#else
import qualified Data.HashMap.Strict as KeyMap
import qualified Data.Text as T

keyFromString :: String -> T.Text
keyFromString = T.pack
#endif

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
            if fromMaybe False $ KeyMap.lookup (keyFromString $ show capability) map'
              then Set.insert capability set
              else set
          ) Set.empty [Voice, SMS, MMS]
  parseJSON _ = mzero
