{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FoldsAndTraversalsExercises where

import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.QQ
import Data.IORef
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Internal
import Data.Vector

users :: Value
users =
  [aesonQQ|
  {
    "users": [
      {
        "name": "qiao.yifan",
        "email": "qyifan@xingxin.com",
        "metadata": {
          "num_logins": 5
        }
      },
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 27,
          "associated_ips": [
            "52.49.1.233",
            "52.49.1.234"
          ]
        }
      },
      {
        "name": "su.mucheng",
        "email": "smucheng@xingxin.com",
        "metadata": {
          "associated_ips": [
            "51.2.244.193"
          ]
        }
      }
    ]
  }
|]

-- I.
a :: [Text]
a = users ^.. key "users" . values . key "name" . _String

-- ["qiao.yifan","ye.xiu","su.mucheng"]

b :: [Text]
b = users ^.. key "users" . values . key "email" . _String

-- ["qyifan@xingxin.com","smucheng@xingxin.com"]

c :: [Text]
c = users ^.. key "users" . _Array . traversed . key "email" . _String

-- ["qyifan@xingxin.com","smucheng@xingxin.com"]

d :: [Text]
d = users ^.. key "users" . _Array . folded . key "email" . _String

-- ["qyifan@xingxin.com","smucheng@xingxin.com"]

e :: Text
e = users ^. key "users" . values . key "name" . _String

-- "qiao.yifanye.xiusu.mucheng"

f :: Value
f = users & key "users" . values . key "name" . _String %~ Text.toUpper

-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "QIAO.YIFAN"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "YE.XIU"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "SU.MUCHENG"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])

g :: [Vector Value]
g =
  users
    ^.. key "users"
      . values
      . key "metadata"
      . key "associated_ips"
      . _Array

-- [[String "52.49.1.233",String "52.49.1.234"],[String "51.2.244.193"]]

h :: [Text]
h =
  users
    ^.. key "users"
      . values
      . key "metadata"
      . key "associated_ips"
      . values
      . _String

-- ["52.49.1.233","52.49.1.234","51.2.244.193"]

i :: Integer
i =
  users
    & foldlOf
      (key "users" . values . key "metadata" . key "num_logins" . _Integer)
      (+)
      0

-- 32

j :: Any
j =
  users
    & foldMapOf
      (key "users" . _Array . folded . key "name" . _String)
      (\x -> Any $ Text.length x <= 8)

-- Any {getAny = True}

-- II.

-- | for each username,
-- print it out and prompt you for a replacement name,
-- then return an IO containing the JSON with the updated names.
k :: IO Value
k =
  users
    & traverseOf
      (key "users" . values . key "name" . _String)
      (\x -> print x *> fmap Text.pack getLine)

l :: IO Value
l =
  do
    ref <- newIORef 0
    users
      & traverseOf
        ( key "users"
            . _Array
            . traversed
            . key "metadata"
            . key "num_logins"
            . _Integer
        )
        (\x -> modifyIORef' ref (+ x) *> readIORef ref)

m :: IO Value
m =
  users
    & traverseOf
      (key "users" . values . key "email" . _String)
      (\x -> Text.putStrLn x *> pure (Text.reverse x))

getAliasMay :: Text -> Maybe Text
getAliasMay "ye.xiu" = Just "ye.qiu"
getAliasMay _ = Nothing

-- getAliasMay x        = Just x

n :: Maybe Value
n =
  users
    & traverseOf
      (key "users" . values . key "name" . _String)
      getAliasMay
