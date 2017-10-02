module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "order3" $ do
        order3 (3, 2, 1) `shouldBe` (1, 2, 3)
        order3 (2, 2, 3) `shouldBe` (2, 2, 3)
        order3 (5, 2, 10) `shouldBe` (2, 5, 10)

    it "highestBit" $ do
        highestBit 15 `shouldBe` (8, 3)
        highestBit 16 `shouldBe` (16, 4)
        highestBit 17 `shouldBe` (16, 4)

    it "smartReplicate" $ do
         smartReplicate [1,2,3] `shouldBe` [1,2,2,3,3,3]


