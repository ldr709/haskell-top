module Control.Monad.Partial
  ( Partial()
  , nontotalRunPartial
  , contSeq
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Fail as Fail
import Control.Monad.Zip
import Data.Unamb

-- Values in haskell are partial, but this is still useful for using types to
-- document an API. Adds an additional layer of laziness to the type.
-- TODO: Hide implementation details, maybe allow pure Haskell implementation.
data Partial a = Partial { nontotalRunPartial :: a }

instance Functor Partial where
  fmap f = Partial . f . nontotalRunPartial

instance Applicative Partial where
  pure = Partial
  (<*>) f = Partial . nontotalRunPartial f . nontotalRunPartial
  (*>) = liftA2 seq
  (<*) = flip (*>)

-- Lift seq into ContT
contSeq :: ContT a Partial x -> ContT a Partial x -> ContT a Partial x
contSeq u v = ContT $ \c -> runContT u c *> runContT v c

instance Monad Partial where
  x >>= f = fmap (nontotalRunPartial . f) x
  fail = Fail.fail

instance Fail.MonadFail Partial where
  fail = return . error

-- Note: mplus is nondeterministic, so if both arguments terminate then both are
-- valid results. TODO: terminate unused computation.
instance Alternative Partial where
  empty = Partial undefined
  (<|>) = liftA2 unamb

instance MonadPlus Partial

instance MonadZip Partial where
  mzipWith = liftA2
