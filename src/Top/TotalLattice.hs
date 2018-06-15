module Top.TotalLattice where

import Algebra.Lattice
import Algebra.SemiBoundedLattice
import Control.Applicative
import Control.Monad
import Control.Monad.Partial
import Data.Int
import Data.Word
import Numeric.Natural
import Top.Apartness

-- Sort of like a pseudo-order. Total means that every element must be ordered
-- with every other, so meet and join must always return one of their two
-- arguments.
--
-- Laws:
-- Totality: not (x \/ y ?/=? x and x \/ y ?/=? y)
--           not (x /\ y ?/=? x and x /\ y ?/=? y)
-- These two laws imply distributivity.
--
-- All functions given default definitions class are for convenience. They may
-- be overridden with better implementations, but those implementations must be
-- equivalent to the ones defined here.
class (Apartness a, DistributiveLattice a) =>
      TotalLattice a
  where

  -- (x ?<? y) should terminate if and only if (x < y).
  infixl 4 ?<?
  (?<?) :: MonadPartial p => a -> a -> p ()
  x ?<? y = x /\ y ?/=? y

  -- pseudoCompare is like Ord's compare: return true if (x < y), false if
  -- (x > y), and not terminate if they are equal.
  pseudoCompare :: MonadPartial p => a -> a -> p Bool
  pseudoCompare x y = whichDifferent (x /\ y) x y

  -- Witness to transitivity. Precondition: y < z. False if (y < x). True if
  -- (x < z). If both are true then it can return either. (At least one must
  -- hold for the precondition to hold). If the precondition fails then it may
  -- or may not halt.
  whichSide :: MonadPartial p => a -> a -> a -> p Bool
  whichSide x y z = whichDifferent (x /\ z \/ y) y z

-- Flipped (?<?).
infixl 4 ?>?
(?>?) :: (MonadPartial p, TotalLattice a) => a -> a -> p ()
(?>?) = flip (?<?)

instance (Ord a, DistributiveLattice a) => TotalLattice (EqApart a) where
  (EqApart x) ?<? (EqApart y) = if x < y then pure () else empty
  pseudoCompare (EqApart x) (EqApart y) =
    case compare x y of
      LT -> pure True
      GT -> pure False
      EQ -> empty
  whichSide (EqApart x) (EqApart y) (EqApart z) =
    if y < z
      then if y < x then return False else return True
      else empty

instance DistributiveLattice ()
instance TotalLattice () where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance DistributiveLattice Bool
instance TotalLattice Bool where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Ordering where
  (/\) = min
instance JoinSemiLattice Ordering where
  (\/) = max
instance Lattice Ordering
instance DistributiveLattice Ordering
instance TotalLattice Ordering where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Char where
  (/\) = min
instance JoinSemiLattice Char where
  (\/) = max
instance Lattice Char
instance DistributiveLattice Char
instance TotalLattice Char where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Int where
  (/\) = min
instance JoinSemiLattice Int where
  (\/) = max
instance Lattice Int
instance DistributiveLattice Int
instance TotalLattice Int where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Int8 where
  (/\) = min
instance JoinSemiLattice Int8 where
  (\/) = max
instance Lattice Int8
instance DistributiveLattice Int8
instance TotalLattice Int8 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Int16 where
  (/\) = min
instance JoinSemiLattice Int16 where
  (\/) = max
instance Lattice Int16
instance DistributiveLattice Int16
instance TotalLattice Int16 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Int32 where
  (/\) = min
instance JoinSemiLattice Int32 where
  (\/) = max
instance Lattice Int32
instance DistributiveLattice Int32
instance TotalLattice Int32 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Int64 where
  (/\) = min
instance JoinSemiLattice Int64 where
  (\/) = max
instance Lattice Int64
instance DistributiveLattice Int64
instance TotalLattice Int64 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Word where
  (/\) = min
instance JoinSemiLattice Word where
  (\/) = max
instance Lattice Word
instance DistributiveLattice Word
instance TotalLattice Word where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Word8 where
  (/\) = min
instance JoinSemiLattice Word8 where
  (\/) = max
instance Lattice Word8
instance DistributiveLattice Word8
instance TotalLattice Word8 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Word16 where
  (/\) = min
instance JoinSemiLattice Word16 where
  (\/) = max
instance Lattice Word16
instance DistributiveLattice Word16
instance TotalLattice Word16 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Word32 where
  (/\) = min
instance JoinSemiLattice Word32 where
  (\/) = max
instance Lattice Word32
instance DistributiveLattice Word32
instance TotalLattice Word32 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Word64 where
  (/\) = min
instance JoinSemiLattice Word64 where
  (\/) = max
instance Lattice Word64
instance DistributiveLattice Word64
instance TotalLattice Word64 where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance Lattice Integer
instance DistributiveLattice Integer
instance TotalLattice Integer where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
instance MeetSemiLattice Natural where
  (/\) = min
instance JoinSemiLattice Natural where
  (\/) = max
instance Lattice Natural
instance DistributiveLattice Natural
instance TotalLattice Natural where
  x ?<? y = (EqApart x) ?<? (EqApart y)
  pseudoCompare x y = pseudoCompare (EqApart x) (EqApart y)
  whichSide x y z = whichSide (EqApart x) (EqApart y) (EqApart z)
