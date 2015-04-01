{-#LANGUAGE OverloadedStrings #-}

module Twilio.Types.PriceUnit where

import Control.Monad
import Data.Aeson
import Data.Text as T

data PriceUnit
  = USD
  | EUR
  | JPY
  | OtherPriceUnit !String
  deriving Eq

instance Show PriceUnit where
  show USD = "USD"
  show EUR = "EUR"
  show JPY = "JPY"
  show (OtherPriceUnit pu) = pu

instance FromJSON PriceUnit where
  parseJSON (String "USD") = return USD
  parseJSON (String "EUR") = return EUR
  parseJSON (String "JPY") = return JPY
  parseJSON (String t) = return . OtherPriceUnit $ T.unpack t
  parseJSON _ = mzero
