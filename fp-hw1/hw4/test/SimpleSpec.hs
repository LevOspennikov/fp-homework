module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "foldr" $ do
        foldr (+) 0 (fromList [1, 3, 2]) `shouldBe` 6
        foldr (*) 1 (fromList [1, 3, 2]) `shouldBe` 6

    it "foldMap" $ do
        sad (foldMap wow (fromList [1, 2, 3])) `shouldBe` 6

    it "splitOn" $ do
        splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]

