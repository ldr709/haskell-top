{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Top.Apartness where

import Algebra.Lattice
import Algebra.SemiBoundedLattice
import Control.Applicative
import Control.Monad
import Control.Monad.Partial
import Data.Int
import Data.Word
import Numeric.Natural

-- (x ?/=? y) should terminate if and only if x != y.
--
-- Laws:
-- Reflexivity: not (x ?/=? x)
-- Symmetry: x ?/=? y if and only if y ?/=? x
-- Transitivity: y ?/=? z implies (x ?/=? y or x ?/=? z)
-- Tightness: x is considered equal to y if not x ?/=? y.
class Apartness a where
  infixl 4 ?/=?
  (?/=?) :: MonadPartial p => a -> a -> p ()
  -- whichDifferent is a witness to transitivity.
  -- whichDifferent returns false if x /= y, and true if x /= z. If both hold
  -- then it can pick either. If neither hold then it runs forever.
  --
  -- Default is to use the partiallity monad's choice operator to run both in
  -- parallel until one returns.
  whichDifferent :: MonadPartial p => a -> a -> a -> p Bool
  whichDifferent x y z = x ?/=? y *> pure False <|> x ?/=? z *> pure True

newtype EqApart a = EqApart
  { runEqApart :: a
  } deriving ( Eq
             , Ord
             , Show
             , JoinSemiLattice
             , MeetSemiLattice
             , Lattice
             , DistributiveLattice
             )

instance Eq a => Apartness (EqApart a) where
  (EqApart x) ?/=? (EqApart y) =
    if x == y
      then empty
      else pure ()
  -- Deriving Eq usually means that equality checking is a total function, so
  -- this can be done more easily.
  whichDifferent (EqApart x) (EqApart y) (EqApart z) =
    case (x == y, x == z) of
      (False, _) -> pure False
      (_, False) -> pure True
      (True, True) -> empty

instance Apartness () where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Bool where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Ordering where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Char where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Int where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Int8 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Int16 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Int32 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Int64 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Word where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Word8 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Word16 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Word32 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Word64 where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Integer where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
instance Apartness Natural where
  x ?/=? y = (EqApart x) ?/=? (EqApart y)
  whichDifferent x y z = whichDifferent (EqApart x) (EqApart y) (EqApart z)
