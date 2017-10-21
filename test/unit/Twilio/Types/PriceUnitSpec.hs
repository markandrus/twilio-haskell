{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE InstanceSigs #-}
module Twilio.Types.PriceUnitSpec where

import Test.Hspec
import Twilio.Types.PriceUnit
import Data.Aeson
import Test.QuickCheck hiding (Success)
import Data.Text (Text)
import Test.QuickCheck.Instances ()

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
      it "should work for BTC" $ do
        let expectedResults = (Success $ OtherPriceUnit "BTC")
        fromJSON (String "BTC") `shouldBe` expectedResults
      it "should work for arbitrary strings" $ property $ forAll notPriceUnitEnum $
        \code -> fromJSON (String code) == (Success $ OtherPriceUnit code)

notPriceUnitEnum :: Gen Text
notPriceUnitEnum = suchThat arbitrary $
    \code -> not $ code `elem` ["USD", "EUR", "JPY"]
