{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrismsExercises where

import Data.Aeson (Value)
import Data.Aeson.QQ

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
