module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "eval" $ do
        eval (Minus (Constant 3) (Plus (Constant 4) (Mul (Constant 2) (Pow (Constant 1) (Div (Constant 4) (Constant 2)))))) `shouldBe` Right (-3)
        eval (Pow (Constant 3) (Constant (-1))) `shouldBe` Left ExponentiationInNegative
        eval (Div (Constant 3) (Constant (0))) `shouldBe` Left DivisionByZero
