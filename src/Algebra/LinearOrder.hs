module Algebra.LinearOrder where

import Algebra.Apartness
import Algebra.PartialOrd
import Algebra.QuasiOrder
import Control.Applicative
import Control.Monad.Partial
import Data.Int
import Data.Word
import Numeric.Natural

-- Laws:
-- * Comparison: if x ?<? z then x ?<? y or y ?<? z.
-- * Apartness:  x ?<? y or y ?<? x if and only if x ?/=? y
--
-- whichSide and linearCompare must be equivalent to the default definitions
-- given here.
class (Apartness a, QuasiOrder a) => LinearOrder a where
  -- Witness to Comparison. Precondition: x ?<? z. True if (x ?<? y). False if
  -- (y ?<? z). If both are true then it can return either. (At least one must
  -- hold for the precondition to hold). If the precondition fails then it may
  -- or may not halt.
  whichSide :: MonadPartial p => a -> a -> a -> p Bool
  whichSide x y z = x ?<? y *> pure True <|> y ?<? z *> pure False

  -- Witness to Apartness. linearCompare is like Ord's compare: return true if
  -- (x ?<? y), false if (x ?>? y), and don't terminate if they are equal.
  linearCompare :: MonadPartial p => a -> a -> p Bool
  linearCompare x y = x ?<? y *> pure True <|> y ?<? x *> pure False

instance (PartialOrd a, Ord a) => LinearOrder (TotalWrap a) where
  linearCompare (TotalWrap x) (TotalWrap y) =
    case compare x y of
      LT -> pure True
      GT -> pure False
      EQ -> empty
  whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z) =
    if x < z
      then pure $ x < y
      else empty

instance LinearOrder () where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Bool where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Ordering where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Char where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Int where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Int8 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Int16 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Int32 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Int64 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Word where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Word8 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Word16 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Word32 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Word64 where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Integer where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
instance LinearOrder Natural where
  linearCompare x y = linearCompare (TotalWrap x) (TotalWrap y)
  whichSide x y z = whichSide (TotalWrap x) (TotalWrap y) (TotalWrap z)
