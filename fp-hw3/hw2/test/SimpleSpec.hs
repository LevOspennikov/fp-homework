module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
   it "set _1  6 (1, 3)" $
      set _1  6 (1, 3) `shouldBe` (6, 3)
   it "set _2 6 (1, 5)" $
      set _2 6 (1, 5) `shouldBe` (1, 6)
   it "view _2 (1, 5)" $
      view _2 (1, 5) `shouldBe` 5
   it "view _1 (1, 5)" $
      view _1 (1, 5) `shouldBe` 1
   it "set _2 6 (1, 5)" $
      over _2 (+1) (1, 5) `shouldBe` (1, 6)
