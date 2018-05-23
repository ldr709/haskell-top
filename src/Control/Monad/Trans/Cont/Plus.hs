module Control.Monad.Trans.Cont.Plus where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont

instance Alternative m => Alternative (ContT a m) where
  empty = ContT $ const empty
  x <|> y = ContT $ \c -> runContT x c <|> runContT y c

instance MonadPlus m => MonadPlus (ContT a m)
