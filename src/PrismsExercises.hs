{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrismsExercises where

import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.QQ
import qualified Data.Text as Text
import Data.Text.Internal
import Data.Vector

-- :t key
-- key
--   :: (AsValue t, Applicative f) =>
--      Text -> (Value -> f Value) -> t -> f t
-- :t (^?)
-- (^?) :: s -> Getting (Data.Monoid.First a) s a -> Maybe a

user1 :: Value
user1 =
  [aesonQQ|
  {
    "name": "qiao.yifan",
    "email": "qyifan@xingxin.com"
  }
|]

user2 :: Value
user2 =
  [aesonQQ|
  {
    "name": "ye.xiu",
    "metadata": {
      "num_logins": 27
    }
  }
|]

-- I
a :: Maybe Integer
a = user1 ^? key "metadata".key "num_logins"._Integer
-- Nothing

b :: Maybe Data.Text.Internal.Text
b = user1 ^? key "email"._String
-- Just "qyifan@xingxin.com"

c :: Maybe Text
c = user2 ^? key "email"._String
-- Nothing

d :: Maybe (Vector Value)
d = user2 ^? key "metadata".key "associated_ips"._Array
-- Nothing

e :: Vector Value
e = user2 ^. key "metadata".key "associated_ips"._Array
-- []

f :: Maybe Text
f = user1 ^? key "name"._String.to Text.toUpper
-- ust "QIAO.YIFAN"

g :: Value
g = user1 & key "name"._String .~ "su.mucheng"
-- Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "su.mucheng")])

h :: Value
h = user2 & key "email"._String .~ "yxiu@xingxin.com"
-- Object (fromList [("name",String "ye.xiu"),("metadata",Object (fromList [("num_logins",Number 27.0)]))])

i :: Value
i = user2 & key "name"._String %~ Text.reverse
-- Object (fromList [("name",String "uix.ey"),("metadata",Object (fromList [("num_logins",Number 27.0)]))])
