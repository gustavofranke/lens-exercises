{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module PrismsSpec where

import PrismsExercises
import Test.Hspec

spec :: Spec
spec = do
  describe "Ex I" $ do
    it "can I" $ do
      a `shouldBe` Nothing
      b `shouldBe` Just "qyifan@xingxin.com"
      c `shouldBe` Nothing
      d `shouldBe` Nothing
      f `shouldBe` Just "QIAO.YIFAN"
  describe "Ex II" $ do
    it "can II" $ do
      j `shouldBe` Just 27
