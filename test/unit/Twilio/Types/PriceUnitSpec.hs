{-#LANGUAGE OverloadedStrings #-}
module Twilio.Types.PriceUnitSpec where

import Test.Hspec
import Twilio.Types.PriceUnit
import Data.Aeson

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PriceUnit" $ do
    describe "decoding from JSON" $ do
      it "should work for USD" $ do
        fromJSON (String "USD") `shouldBe` (Success USD)
      it "should work for EUR" $ do
        fromJSON (String "EUR") `shouldBe` (Success EUR)
      it "should work for JPY" $ do
        fromJSON (String "JPY") `shouldBe` (Success JPY)
      it "should work for arbitrary string" $ do
        let expectedResults = (Success $ OtherPriceUnit "BTC")
        fromJSON (String "BTC") `shouldBe` expectedResults
