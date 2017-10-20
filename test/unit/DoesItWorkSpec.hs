module DoesItWorkSpec where

import Test.Hspec

spec :: Spec
spec = do
    describe "testing" $ do
        it "works" $ do
            1 `shouldBe` 1
