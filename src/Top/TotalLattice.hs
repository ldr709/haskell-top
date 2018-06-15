module Top.TotalLattice where

import Algebra.Lattice
import Algebra.SemiBoundedLattice
import Control.Applicative
import Control.Monad
import Control.Monad.Partial
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
