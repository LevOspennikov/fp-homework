module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "nextDay" $ do
        fromEnum (nextDay (toEnum (5::Int) ::WeekDay)) `shouldBe` 6
        fromEnum (nextDay (toEnum (6::Int) ::WeekDay)) `shouldBe` 0

    it "afterDay" $ do
        fromEnum (afterDays (toEnum 0) 7) `shouldBe` 0
        fromEnum (afterDays (toEnum 0) 8) `shouldBe` 1

    it "isWeekend" $ do
        isWeekend (toEnum 3) `shouldBe` False
        isWeekend (toEnum 6) `shouldBe` True

    it "daysToParty" $ do
        daysToParty (toEnum 4) `shouldBe` 0
        daysToParty (toEnum 5) `shouldBe` 6

    it "vecLen" $ do
        vecLen (toVec3d 1 2 2) `shouldBe` 3
        vecLen (toVec2d 3 4) `shouldBe` 5

    it "vecAdd" $ do
        vecLen (vecAdd (toVec3d 0 0 2) (toVec2d 1 2)) `shouldBe` 3
        vecLen (vecAdd (toVec3d 0 0 2) (toVec3d 1 2 0)) `shouldBe` 3

    it "vecCrossProd" $ do
        vecCrossProd (toVec3d 1 1 1) (toVec2d 1 1) `shouldBe` 2
        vecCrossProd (toVec3d 1 1 1) (toVec3d 1 1 1) `shouldBe` 3

    it "vecDist" $ do
        vecDist (toVec3d 0 0 2) (toVec2d (-1) (-2)) `shouldBe` 3

    it "vecDotProd" $ do
        vecLen (vecDotProd (toVec3d 1 2 3) (toVec3d 3 2 1)) `shouldBe` vecLen (toVec3d (-4) 8 (-4))

    it "addTwoNats" $ do
        natToInt (addTwoNats (intToNat 4) (intToNat 5)) `shouldBe` 9
        natToInt (addTwoNats (intToNat 0) (intToNat 5)) `shouldBe` 5

    it "minusTwoNats" $ do
        natToInt (minusTwoNats (intToNat 5) (intToNat 5)) `shouldBe` 0
        natToInt (minusTwoNats (intToNat 10) (intToNat 5)) `shouldBe` 5

    it "mulTwoNats" $ do
        natToInt (mulTwoNats (intToNat 0) (intToNat 5)) `shouldBe` 0
        natToInt (mulTwoNats (intToNat 5) (intToNat 5)) `shouldBe` 25

    it "eqTwoNats" $ do
        intToNat 4 > intToNat 5 `shouldBe` False
        intToNat 4 == intToNat 4 `shouldBe` True
        intToNat 5 == intToNat 6 `shouldBe` False
        intToNat 4 >= intToNat 4 `shouldBe` True

    it "empty" $ do
        empty (fromList [1]) `shouldBe` False
        empty (fromList [1, 2, 3]) `shouldBe` False

    it "sizeTree" $ do
        sizeTree (fromList [1, 2, 3]) `shouldBe` 3
        sizeTree (fromList [1, 2, 3, 4]) `shouldBe` 4

    it "contains" $ do
        contains (fromList [1, 2, 3]) 3 `shouldBe` True
        contains (fromList [1, 2, 3]) 4 `shouldBe` False

    it "insert" $ do
        contains (insert (fromList [1, 2]) 0) 3 `shouldBe` False
        sizeTree (insert (fromList [1, 2]) 3) `shouldBe` 3

