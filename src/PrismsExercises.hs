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
a = user1 ^? key "metadata" . key "num_logins" . _Integer

b :: Maybe Data.Text.Internal.Text
b = user1 ^? key "email" . _String

c :: Maybe Text
c = user2 ^? key "email" . _String

d :: Maybe (Vector Value)
d = user2 ^? key "metadata" . key "associated_ips" . _Array

e :: Vector Value
e = user2 ^. key "metadata" . key "associated_ips" . _Array

f :: Maybe Text
f = user1 ^? key "name" . _String . to Text.toUpper

g :: Value
g = user1 & key "name" . _String .~ "su.mucheng"

h :: Value
h = user2 & key "email" . _String .~ "yxiu@xingxin.com"

i :: Value
i = user2 & key "name" . _String %~ Text.reverse

-- II
-- j = user2 ^. key "metadata".key "num_logins"._Integer
--     • No instance for (Monoid Integer) arising from a use of ‘key’
--     • In the first argument of ‘(.)’, namely ‘key "metadata"’
--       In the second argument of ‘(^.)’, namely
--         ‘key "metadata" . key "num_logins" . _Integer’
--       In the expression:
--         user2 ^. key "metadata" . key "num_logins" . _Integer
j :: Maybe Integer
j = user2 ^? key "metadata" . key "num_logins" . _Integer

k :: Value
k = user1 & key "metadata" . key "num_logins" . _Integer .~ 25

-- l = user2 & key "metadata".key "num_logins"._Integer %~ (+ 1)
-- • No instance for (Num Value) arising from a use of ‘+’
-- • In the second argument of ‘(%~)’, namely ‘(+ 1)’
--   In the second argument of ‘(&)’, namely
--     ‘key "metadata" . key "num_logins" %~ (+ 1)’
--   In the expression:
--     user2 & key "metadata" . key "num_logins" %~ (+ 1)
l :: Value
l = user2 & key "metadata" . key "num_logins" . _Integer %~ (+ 1)

-- m = user1 ^. key "email"
-- • No instance for (Monoid Value) arising from a use of ‘key’
-- • In the second argument of ‘(^.)’, namely ‘key "email"’
--   In the expression: user1 ^. key "email"
--   In an equation for ‘m’: m = user1 ^. key "email"
m :: Maybe Value
m = user1 ^? key "email"

-- n = user2 & key "name"._String .~ 50
-- • No instance for (Num Text) arising from the literal ‘50’
-- • In the second argument of ‘(.~)’, namely ‘50’
--   In the second argument of ‘(&)’, namely
--     ‘key "name" . _String .~ 50’
--   In the expression: user2 & key "name" . _String .~ 50
n :: Value
n = user2 & key "name" . _String .~ "50"
