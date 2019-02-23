module Analysis.Real where

import Algebra.Field
import Algebra.LinearLattice
import Topology.Compact

-- Laws:
-- x < y implies x + z < y + z
-- 0 < x and 0 < y implies 0 < x * y
class (LinearLattice a, Field a) => RealOrderedField a

-- fromRational must be an injection.
-- Division by zero is undefined / doesn't terminate.
-- abs x == x \/ negate x
-- signum == x / abs x        (Note: undefined if x == 0)
-- The Functions in Floating must satisfy the usual definitions.
class (Overt a, RealOrderedField a, Floating a) =>
      Real a

-- Want a few different definitions of real numbers.
-- A few kinds of Cauchy sequences.
--     Some variant with support for using float and double.
-- Bases smaller than the number of options. Base golden ratio. Base 2: -1 0 +1.
-- Dedekind cuts.
-- Continued fractions:
--     Rationals: finite length
--     Irrationals: infinite length
--     Reals: Unknown length continued fractions??
-- Memoization.

-- Also want some way of doing backwards analysis, so that discontinuous
-- functions can be evaluated in the sense of being close to the correct answer
-- for almost the right inputs.
