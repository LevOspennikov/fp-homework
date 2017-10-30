module SimpleSpec where

import           Lib

import           Test.Hspec

spec :: Spec
spec = do
    it "parser" $ do
        runParser (fmap (+2) posInt) "345" `shouldBe`  Just (347, "")
        runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
        runParser abParser "aebcdef" `shouldBe` Nothing
        runParser abParser "ab" `shouldBe` Just (('a', 'b'), "")

