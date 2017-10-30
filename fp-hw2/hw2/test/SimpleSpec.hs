module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "bin" $ do
        bin 1 `shouldBe` [[1],[0]]
        length (bin 2) `shouldBe` 4
        length (bin 10) `shouldBe` 1024

    it "combinations" $ do
        combinations 3 1 `shouldBe` [[3],[2],[1]]
        combinations 3 2 `shouldBe` [[3,2],[3,1],[2,1]]
        length(combinations 8 4) `shouldBe` 70

    it "permutations" $ do
        permutations [1, 2] `shouldBe` [[1,2],[2,1]]
        length (permutations [1..5]) `shouldBe` 120

