{-# LANGUAGE MultiParamTypeClasses,
    FlexibleInstances #-}

module Capsir.Ids
    ( UniqueId
    , UniqueIdT
    , runUniqueId
    , runUniqueIdT
    , newId
    , newRef
    , refId
    , refValue
    , Ref()
    , ID()
    ) where

import Prelude hiding (id)
import Control.Monad.State
import Control.Monad.Identity

newtype ID = ID { getIndex :: Int }
  deriving (Show, Eq)

newtype UniqueIdT m a = UniqueIdT { getInner :: StateT ID m a }

instance Monad m => Monad (UniqueIdT m) where
  a >>= b = UniqueIdT $ getInner a >>= \s -> getInner (b s)
  return a = UniqueIdT $ return a

instance MonadTrans UniqueIdT where
  lift = UniqueIdT . lift

instance MonadFix m => MonadFix (UniqueIdT m) where
  mfix f = UniqueIdT $ mfix $ getInner . f

class Monad m => MonadUniqueId m where
  newId :: m ID

instance Monad m => MonadUniqueId (UniqueIdT m) where
  newId = UniqueIdT $ do
      curr <- get
      put $ ID $ 1 + getIndex curr
      return curr

instance MonadUniqueId m => MonadUniqueId (StateT s m) where
  newId = lift newId

instance MonadIO m => MonadIO (UniqueIdT m) where
  liftIO = UniqueIdT . liftIO

runUniqueIdT :: Monad m => UniqueIdT m a -> m a
runUniqueIdT uid = evalStateT (getInner uid) (ID 0)

type UniqueId = UniqueIdT Identity

runUniqueId :: UniqueId a -> a
runUniqueId = runIdentity . runUniqueIdT

data Ref a = Ref { refId :: ID, refValue :: a }

instance Functor Ref where
  fmap f (Ref id v) = Ref id (f v)

newRef :: MonadUniqueId m => a -> m (Ref a)
newRef v = do
  id <- newId
  return $ Ref id v

