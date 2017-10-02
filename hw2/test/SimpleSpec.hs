module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "removeAt" $ do
        removeAt 1 [1,2,3] `shouldBe` [1, 3]
        removeAt 10 [1,2,3] `shouldBe` [1,2,3]
        removeAt 2 "abc" `shouldBe` "ab"

    it "collectEvery" $ do
        collectEvery 3 [1..8] `shouldBe` ([1,2,4,5,7,8], [3,6])

    it "stringSum" $ do
        stringSum "1 1" `shouldBe` 2
        stringSum "100\n\t-3" `shouldBe` 97

    it "mergeSort" $ do
        mergeSort [2, 1, 0, 3, 10, 5] `shouldBe` [0, 1, 2, 3, 5, 10]
        mergeSort [1, 1, 1, 2, 2, 3] `shouldBe` [1, 1, 1, 2, 2, 3]

