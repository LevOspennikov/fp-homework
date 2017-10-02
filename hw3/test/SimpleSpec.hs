module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "dayNum" $ do
        nextDay Lib.WeekDay.Sunday `shouldBe` Lib.WeekDay.Monday

