{-# LANGUAGE OverloadedStrings #-}
module FoldsAndTraversalsSpec where

import FoldsAndTraversalsExercises
import Test.Hspec

spec :: Spec
spec = do
  describe "Ex I" $ do
    it "can I" $ do
      a `shouldBe` ["qiao.yifan","ye.xiu","su.mucheng"]
      b `shouldBe` ["qyifan@xingxin.com","smucheng@xingxin.com"]
      c `shouldBe` ["qyifan@xingxin.com","smucheng@xingxin.com"]
      d `shouldBe` ["qyifan@xingxin.com","smucheng@xingxin.com"]
      e `shouldBe` "qiao.yifanye.xiusu.mucheng"
