{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Partial where

import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.Trans.Cont
import Data.Functor.Identity
import Data.Unamb

-- A MonadPartial is a Monad that represents non-total computations inside the
-- context of a Monad m. Values in haskell are partial, but it is still useful
-- to specify what types aren't total, and there are multiple implementations of
-- MonadPartial. return and lift may be strict, so if you want to turn a
-- unevaluated lazy value into a partial value use lazy.
--
-- The Applicative instance's (*>) and (<*) must return a computation that
-- doesn't complete unless both arguments complete. That is, they must act like
-- seq. Similarly, the Functor's (<$) must force both its arguments.
--
-- MonadPlus's mempty is a non-total computation. mplus evaluates both arguments
-- in parallel, then picks the one that terminates. If both do it can pick
-- either (nondeterministically). If neither do then it doesn't terminate. It is
-- valid to use infinite recursion on <|>, like (x = x <|> y). It will still
-- terminate if any thing that gets <|> termintes.
class (Monad m, MonadPlus p, Fail.MonadFail p) =>
      MonadPartialT m p | p -> m where

  -- Like lift, but not strict.
  lazyT :: m a -> p a

  -- Evaluate the partial computation. Doesn't always halt.
  nontotalRunPartialT :: p a -> m a

-- Convenience wrapper for MonadPartialT Identity.
class MonadPartialT Identity p => MonadPartial p where
  lazy :: a -> p a
  lazy = lazyT . Identity

  nontotalRunPartial :: p a -> a
  nontotalRunPartial = runIdentity . nontotalRunPartialT

-- Lift seq into ContT
contSeq :: MonadPartialT m p => ContT a p x -> ContT a p x -> ContT a p x
contSeq u v = ContT $ \c -> runContT u c *> runContT v c
