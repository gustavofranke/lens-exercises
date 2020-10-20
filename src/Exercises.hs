{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises where -- TODO: rename to LensesExercises

import Data.Text hiding (reverse, head)
import Control.Lens

-- :t (^.)
-- (^.) :: s -> Getting a s a -> a
-- > :t view
-- view
--   :: mtl-2.2.2:Control.Monad.Reader.Class.MonadReader s m =>
--      Getting a s a -> m a
-- 
-- :t (&)
-- (&) :: a -> (a -> b) -> b
-- 
-- :t (.~)
-- (.~) :: ASetter s t a b -> b -> s -> t
-- > :t set
-- set :: ASetter s t a b -> b -> s -> t
-- 
-- :t (%~)
-- (%~) :: ASetter s t a b -> (a -> b) -> s -> t
-- > :t over
-- over :: ASetter s t a b -> (a -> b) -> s -> t

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo

user1 :: User
user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

-- I
a :: Text
a = user1 ^. name
-- "qiao.yifan"

b :: Int
b = user1 ^. metadata.numLogins
-- 20

c :: User
c = user1 & metadata.numLogins .~ 0
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 0, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}
--  :set -XOverloadedStrings

d :: User
d = user1 & metadata.associatedIPs %~ ("192.168.0.2" :)
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["192.168.0.2","52.39.193.61","52.39.193.75"]}}

e :: User
e = metadata.numLogins %~ (+ 1) $ user1
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 21, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}

