{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises where

import Data.Text
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