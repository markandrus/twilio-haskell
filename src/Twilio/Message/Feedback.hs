{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Message.Feedback
  ( -- * Resource
    Feedback(..)
    -- * Types
  , Outcome(..)
  ) where

import Control.Monad
import Data.Aeson
import Data.Data
import GHC.Generics

{- Resource -}

data Feedback = Feedback
  { outcome :: !Outcome
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

data Outcome
  = Unconfirmed
  | Confirmed
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance ToJSON Outcome where
  toJSON Unconfirmed = String "unconfirmed"
  toJSON Confirmed = String "confirmed"

instance FromJSON Outcome where
  parseJSON (String "unconfirmed") = pure Unconfirmed
  parseJSON (String "confirmed") = pure Confirmed
  parseJSON _ = mzero
