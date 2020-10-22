{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LensesExercises where

import Control.Lens
import Data.Text hiding (head, reverse)

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
  { _name :: Text,
    _userid :: Int,
    _metadata :: UserInfo
  }
  deriving (Show, Eq)

data UserInfo = UserInfo
  { _numLogins :: Int,
    _associatedIPs :: [Text]
  }
  deriving (Show, Eq)

makeLenses ''User
makeLenses ''UserInfo

user1 :: User
user1 =
  User
    { _name = "qiao.yifan",
      _userid = 103,
      _metadata =
        UserInfo
          { _numLogins = 20,
            _associatedIPs =
              [ "52.39.193.61",
                "52.39.193.75"
              ]
          }
    }

-- I
a :: Text
a = user1 ^. name

b :: Int
b = user1 ^. metadata . numLogins

c :: User
c = user1 & metadata . numLogins .~ 0

d :: User
d = user1 & metadata . associatedIPs %~ ("192.168.0.2" :)

e :: User
e = metadata . numLogins %~ (+ 1) $ user1

-- II

-- * Exercises Control.Lens> user1 & email .~ "qyifan@xingxin.com"

-- <interactive>:27:9: error:
--     Variable not in scope: email :: ASetter User b a0 [Char]
x1 :: User
x1 = user1 & name .~ "qyifan@xingxin.com"

f :: User
f = user1 & metadata .~ UserInfo 17 []

g :: User
g = userid .~ -1 $ user1

-- * Exercises Control.Lens> metadata.associatedIPs .~ [ "50.193.0.23" ] & user1

-- <interactive>:30:47: error:
--     • Couldn't match expected type ‘(User -> User) -> b’
--                   with actual type ‘User’
--     • In the second argument of ‘(&)’, namely ‘user1’
--       In the expression:
--         metadata . associatedIPs .~ ["50.193.0.23"] & user1
--       In an equation for ‘it’:
--           it = metadata . associatedIPs .~ ["50.193.0.23"] & user1
--     • Relevant bindings include it :: b (bound at <interactive>:30:1)
x2 :: User
x2 = user1 & metadata . associatedIPs .~ ["50.193.0.23"]

x2' :: User
x2' = metadata . associatedIPs .~ ["50.193.0.23"] $ user1

-- * Exercises Control.Lens> user1 ^. numLogins.metadata

-- <interactive>:31:10: error:
--     • Couldn't match type ‘UserInfo’ with ‘User’
--       Expected type: Getting UserInfo User UserInfo
--         Actual type: (UserInfo -> Const UserInfo UserInfo)
--                      -> UserInfo -> Const UserInfo UserInfo
--     • In the second argument of ‘(^.)’, namely ‘numLogins . metadata’
--       In the expression: user1 ^. numLogins . metadata
--       In an equation for ‘it’: it = user1 ^. numLogins . metadata

-- <interactive>:31:20: error:
--     • Couldn't match type ‘User’ with ‘Int’
--       Expected type: (UserInfo -> Const UserInfo UserInfo)
--                      -> Int -> Const UserInfo Int
--         Actual type: (UserInfo -> Const UserInfo UserInfo)
--                      -> User -> Const UserInfo User
--     • In the second argument of ‘(.)’, namely ‘metadata’
--       In the second argument of ‘(^.)’, namely ‘numLogins . metadata’
--       In the expression: user1 ^. numLogins . metadata
x3 :: Int
x3 = user1 ^. metadata . numLogins

--  III

-- | Get the associated IP addresses.
h :: [Text]
h = user1 ^. metadata . associatedIPs

-- | Update the user so that the associated IP addresses are in reverse order.
i :: User
i = user1 & metadata . associatedIPs %~ reverse

-- | Update the user so that each word in the name is capitalized.
j :: User
j = user1 & name %~ toUpper

-- | Set the number of logins to 1.
k :: User
k = user1 & metadata . numLogins .~ 1

k' :: User
k' = user1 & metadata . numLogins %~ (+ 1)

-- | Remove all associated IP addresses except the first.
l :: User
l = user1 & metadata . associatedIPs %~ (\ips -> [head ips])
