{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Partial.Lazy
  ( PartialT()
  , Partial()
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.Partial
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Unamb

-- Use haskell's built in laziness to implement MonadPartial.
data PartialT m a = PartialT
  { runPartialT :: m a
  }

type Partial = PartialT Identity

instance (Functor m) => Functor (PartialT m) where
  fmap f = PartialT . fmap f . runPartialT
  (<$) = fmap . flip seq

instance (Applicative m) => Applicative (PartialT m) where
  pure = PartialT . pure
  liftA2 f x y = PartialT $ liftA2 f (runPartialT x) (runPartialT y)
  (<*>) f = PartialT . (runPartialT f <*>) . runPartialT
  (*>) = liftA2 seq
  (<*) = liftA2 (flip seq)

instance (Monad m) => Monad (PartialT m) where
  x >>= f = PartialT $ runPartialT x >>= (runPartialT . f)
  fail = Fail.fail

instance MonadTrans PartialT where
  lift = PartialT

instance (Monad m) => Fail.MonadFail (PartialT m) where
  fail = PartialT . error

instance (Applicative m) => Alternative (PartialT m) where
  empty = PartialT undefined
  -- TODO: terminate unused computation.
  (<|>) = liftA2 unamb

instance (Monad m) => MonadPlus (PartialT m)

instance MonadPartial Partial where
  lazy = PartialT . Identity
  nontotalRunPartial = runIdentity . runPartialT
