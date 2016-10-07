{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Monad
import Data.Aeson
import Data.Text

newtype Test = Test { getTest :: Maybe Text }
  deriving Show

instance FromJSON Test where
  parseJSON = withObject "Test" (\v -> Test <$> v .:? "test")

test1 :: Maybe Test
test1 = decode "{\"test\":\"test\"}"

test2 :: Maybe Test
test2 = decode "{\"test\":null}"

test3 :: Maybe Test
test3 = decode "{}"
