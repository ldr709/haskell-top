module Top.TotalLattice where

import Algebra.Lattice
import Algebra.SemiBoundedLattice
import Control.Applicative
import Control.Monad
import Control.Monad.Partial
import Top.Apartness

-- Sort of like a pseudo-order.
--
-- Laws:
-- Totality: not (x \/ y ?/=? x and x \/ y ?/=? y)
--           not (x /\ y ?/=? x and x /\ y ?/=? y)
-- These two laws imply distributivity.
--
-- All functions defined directly in this class are for convenience. They may be
-- overridden with better implementations, but those implementations must be
-- equivalent to the ones defined here.
class (Apartness a, DistributiveLattice a) =>
      TotalLattice a
  where

  -- (x ?<? y) should terminate if and only if (x < y).
  infixl 4 ?<?
  (?<?) :: a -> a -> Partial ()
  x ?<? y = x /\ y ?/=? y

  -- pseudoCompare is like Ord's compare: return true if (x < y), false if
  -- (x > y), and not terminate if they are equal.
  pseudoCompare :: a -> a -> Partial Bool
  pseudoCompare x y = whichDifferent (x /\ y) x y

  -- Witness to transitivity. Precondition: y < z. False if (y < x). True if
  -- (x < z). If both are true then it can return either. (At least one must
  -- hold).
  whichSide :: a -> a -> a -> Bool
  whichSide x y z = nontotalRunPartial $ whichDifferent (x /\ z \/ y) y z

-- Flipped (?<?).
infixl 4 ?>?
(?>?) :: TotalLattice a => a -> a -> Partial ()
(?>?) = flip (?<?)
