{-# LANGUAGE OverloadedStrings #-}

module LensesSpec where

import LensesExercises
import Test.Hspec

spec :: Spec
spec = do
  describe "Exercise I" $ do
    it "can view the name" $ do
      a `shouldBe` "qiao.yifan"
    it "can view the numLogins" $ do
      b `shouldBe` 20
    it "can set the numLogins to 0" $ do
      c `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 0, _associatedIPs = ["52.39.193.61", "52.39.193.75"]}}
    it "can over an ip to the beginning of associatedIPs" $ do
      d `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["192.168.0.2", "52.39.193.61", "52.39.193.75"]}}
    it "can set the numLogins to +1" $ do
      e `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 21, _associatedIPs = ["52.39.193.61", "52.39.193.75"]}}
  describe "Exercise II" $ do
    it "can set the name to a different one" $ do
      x1 `shouldBe` User {_name = "qyifan@xingxin.com", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61", "52.39.193.75"]}}
    it "can set the metadata with a new UserInfo" $ do
      f `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 17, _associatedIPs = []}}
    it "can set the userid to -1" $ do
      g `shouldBe` User {_name = "qiao.yifan", _userid = -1, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61", "52.39.193.75"]}}
    it "can set the associatedIPs to a new value" $ do
      x2 `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["50.193.0.23"]}}
      x2' `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["50.193.0.23"]}}
    it "can view the numLogins" $ do
      x3 `shouldBe` 20
  describe "Exercise III" $ do
    it "can view IP addresses" $ do
      h `shouldBe` ["52.39.193.61", "52.39.193.75"]
    it "can set associatedIPs in reverse order" $ do
      i `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.75", "52.39.193.61"]}}
    it "can update the user so that each word in the name is capitalized" $ do
      j `shouldBe` User {_name = "QIAO.YIFAN", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61", "52.39.193.75"]}}
    it "can set the number of logins to 1" $ do
      k `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 1, _associatedIPs = ["52.39.193.61", "52.39.193.75"]}}
    it "can increment the number of logins to +1" $ do
      k' `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 21, _associatedIPs = ["52.39.193.61", "52.39.193.75"]}}
    it "can remove all associated IP addresses except the first" $ do
      l `shouldBe` User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61"]}}
