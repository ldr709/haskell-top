module Control.Monad.Partial where

import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.Trans.Cont
import Data.Unamb

-- A MonadPartial is a Monad that represents non-total computations. Values in
-- haskell are partial, but it is still useful to specify what types aren't
-- total, and there are multiple implementations of MonadPartial. return may be
-- strict, so if you want to turn a unevaluated lazy value into a partial value
-- use lazy.
--
-- The Applicative instance's (*>) and (<*) must return a computation that
-- doesn't complete unless both arguments complete. That is, they must act like
-- seq. Similarly, the Functor's (<$) must force both its arguments.
--
-- MonadPlus's mempty is a non-total computation. mplus evaluates both arguments
-- in parallel, then picks the one that terminates. If both do it can pick
-- either (nondeterministically). If neither do then it doesn't terminate.
class (MonadPlus p, Fail.MonadFail p) => MonadPartial p where
  -- Like return, but not strict.
  lazy :: a -> p a

  -- Evaluate the partial computation. Doesn't always halt.
  nontotalRunPartial :: p a -> a

-- Lift seq into ContT
contSeq :: MonadPartial p => ContT a p x -> ContT a p x -> ContT a p x
contSeq u v = ContT $ \c -> runContT u c *> runContT v c
