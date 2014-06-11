module Capsir.Ids
    ( UniqueId
    , newId
    , runUniqueId
    ) where

import Control.Monad.State

newtype UniqueId a = UniqueId { getInner :: State Int a }

instance Monad UniqueId where
  a >>= b = UniqueId $ (getInner a) >>= \s -> getInner (b s)
  return a = UniqueId $ return a

newId :: UniqueId Int
newId = UniqueId $ do
  curr <- get
  put (curr + 1)
  return curr

runUniqueId :: UniqueId a -> a
runUniqueId uid = evalState (getInner uid) 0
