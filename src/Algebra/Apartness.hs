{-# LANGUAGE DefaultSignatures #-}

module Algebra.Apartness where

import {-# SOURCE #-} Algebra.LinearOrder
import Algebra.QuasiOrder
import Control.Applicative
import Control.Monad
import Control.Monad.Partial
import Data.Int
import Data.Word
import Numeric.Natural

-- (x ?/=? y) should terminate if and only if x != y.
--
-- Laws:
-- * Reflexivity:  not (x ?/=? x).
-- * Symmetry:     x ?/=? y if and only if y ?/=? x.
-- * Transitivity: y ?/=? z implies (x ?/=? y or x ?/=? z).
-- * Tightness:    if not (x ?/=? y) then x is considered equal to y.
class Apartness a where
  infixl 4 ?/=?
  (?/=?) :: MonadPartial p => a -> a -> p ()

  default (?/=?) :: (QuasiOrder a, MonadPartial p, LinearOrder a) =>
    a -> a -> p ()
  x ?/=? y = (x ?<? y) <|> (x ?>? y)

  -- whichDifferent is a witness to transitivity.
  -- whichDifferent returns false if x /= y, and true if x /= z. If both hold
  -- then it can pick either. If neither hold then it runs forever.
  --
  -- Default is to use the partiallity monad's choice operator to run both in
  -- parallel until one returns.
  whichDifferent :: MonadPartial p => a -> a -> a -> p Bool
  whichDifferent x y z = x ?/=? y *> pure False <|> x ?/=? z *> pure True

instance Eq a => Apartness (TotalWrap a) where
  (TotalWrap x) ?/=? (TotalWrap y) =
    if x == y
      then empty
      else pure ()
  -- Deriving Eq usually means that equality checking is a total function, so
  -- this can be done more easily.
  whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z) =
    case (x == y, x == z) of
      (False, _) -> pure False
      (_, False) -> pure True
      (True, True) -> empty

instance Apartness () where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Bool where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Ordering where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Char where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Int where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Int8 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Int16 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Int32 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Int64 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Word where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Word8 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Word16 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Word32 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Word64 where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Integer where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)

instance Apartness Natural where
  x ?/=? y = (TotalWrap x) ?/=? (TotalWrap y)
  whichDifferent x y z =
    whichDifferent (TotalWrap x) (TotalWrap y) (TotalWrap z)
