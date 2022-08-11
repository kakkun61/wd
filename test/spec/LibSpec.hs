module LibSpec where

import Lib

import Test.Hspec

spec :: Spec
spec = do
  describe "hello" $ do
    it "pepe" $ do
      hello "Pepe" `shouldBe` "Hello, Pepe!"
