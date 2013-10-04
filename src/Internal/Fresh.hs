{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Internal.Fresh
  ( FreshT (..)
  , Fresh
  , MonadFresh (..)
  , runFreshT
  , runFresh
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

newtype FreshT m a = FreshT { unFreshT :: StateT Int m a }
 deriving (Functor, Applicative, Monad, MonadTrans)

class (Applicative m, Monad m) => MonadFresh m where
  fresh :: m Int

instance (Applicative m, Monad m) => MonadFresh (FreshT m) where
  fresh = FreshT $ modify (+1) >> get

runFreshT :: Monad m => FreshT m a -> m a
runFreshT = flip (evalStateT . unFreshT) 0

type Fresh = FreshT Identity

runFresh :: Fresh a -> a
runFresh = runIdentity . runFreshT

instance (MonadFresh m, Error e) => MonadFresh (ErrorT e m) where
  fresh = lift $ fresh

instance (MonadFresh m) => MonadFresh (ReaderT e m) where
  fresh = lift $ fresh

instance (MonadFresh m) => MonadFresh (StateT s m) where
  fresh = lift $ fresh

instance (MonadFresh m, Monoid e) => MonadFresh (WriterT e m) where
  fresh = lift $ fresh
