{-#LANGUAGE OverloadedStrings #-}

module Twilio.Types.PriceUnit where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

data PriceUnit
  = USD
  | EUR
  | JPY
  | OtherPriceUnit !Text
  deriving Eq

instance Show PriceUnit where
  show USD = "USD"
  show EUR = "EUR"
  show JPY = "JPY"
  show (OtherPriceUnit pu) = T.unpack pu

instance FromJSON PriceUnit where
  parseJSON (String "USD") = return USD
  parseJSON (String "EUR") = return EUR
  parseJSON (String "JPY") = return JPY
  parseJSON (String t) = return $ OtherPriceUnit t
  parseJSON _ = mzero
