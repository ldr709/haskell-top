{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Partial.Pure
  ( PartialT()
  , Partial()
  ) where

import Control.Applicative
import Control.Exception
import Control.Exception.Enclosed
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.Partial
import Control.Monad.Trans.Class
import Data.Functor.Identity
import System.IO.Unsafe
import System.Timeout

data PartialF a x
  = Done a
  | Unfinished x
  -- Non-total computations can finish with an error instead of not terminating.
  | Failure SomeException

-- Pure implementation of Partiality monad, so <|> doesn't require
-- unsafePerformIO (in the form of unamb). This makes <|> deterministic, but
-- still unspecified. Note that this means that lazy must use unsafePerformIO,
-- as they cannot both be implemented
newtype PartialT m a = PartialT
  { runPartialT :: m (PartialF a (PartialT m a))
  }

type Partial = PartialT Identity

unfinished :: Monad m => PartialT m a -> PartialT m a
unfinished = PartialT . return . Unfinished

instance (Monad m) => Functor (PartialT m) where
  fmap = liftM
  (<$) = fmap . flip seq

instance (Monad m) => Applicative (PartialT m) where
  pure = PartialT . pure . Done
  liftA2 = liftM2
  (<*>) = ap
  (*>) = liftA2 seq
  (<*) = liftA2 (flip seq)

instance Monad m => Monad (PartialT m) where
  (PartialT x) >>= f = PartialT $ x >>= go
    where
      go (Done a) = runPartialT $ f a
      go (Unfinished p) = return $ Unfinished (p >>= f)
      go (Failure e) = return $ Failure e
  fail = Fail.fail

instance MonadTrans PartialT where
  lift = PartialT . fmap Done

instance Monad m => Fail.MonadFail (PartialT m) where
  fail = failException . ErrorCall

failException :: (Monad m, Exception e) => e -> PartialT m a
failException = PartialT . return . Failure . toException

instance Monad m => Alternative (PartialT m) where
  empty = Fail.fail "Control.Monad.Partial.Pure.empty"
  -- TODO: Is it possible to use parallelism to speed this up?
  px <|> py =
    unfinished $
    PartialT $ do
      x <- runPartialT px
      case x of
        (Done a) -> return $ Done a
        (Failure _) -> runPartialT py
        (Unfinished px') -> return $ Unfinished $ py <|> px'

instance Monad m => MonadPlus (PartialT m)

instance (Monad m) => MonadPartialT m (PartialT m) where
  lazyT x = unfinished $ unsafePerformIO $ go
    where
      comp = evaluate x
      -- Although timeout kills the thread, it will start where it left off.
      go = do
        result <- tryAny $ timeout 10000 comp
        case result of
          (Left e) -> return $ failException e
          (Right (Just x')) -> return $ lift x'
          (Right (Nothing)) -> unfinished <$> unsafeInterleaveIO go
  nontotalRunPartialT (PartialT mx) = do
    x <- mx
    case x of
      (Done a) -> return a
      (Failure e) -> throw e
      (Unfinished x) -> nontotalRunPartialT x

instance MonadPartial Partial

-- Useful for debugging.
--instance (Show a) => Show (Partial a) where
--  show (PartialT (Identity (Done a))) = "(Done " ++ show a ++ ")"
--  show (PartialT (Identity (Unfinished p))) = "(Unfinished " ++ show p ++ ")"
--  show (PartialT (Identity (Failure f))) = "(Failure " ++ show f ++ ")"
