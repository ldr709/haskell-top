{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.QuasiOrder where

import Algebra.Lattice
import Algebra.Lattice.Ordered
import Algebra.PartialOrd
import Algebra.SemiBoundedLattice
import Control.Applicative
import Control.Monad.Partial
import Data.Int
import Data.Word
import Numeric.Natural

-- A partially computable quasiorder.
--
-- Laws:
-- * Irreflexivity: not x ?<? x.
-- * Transitivity:  x ?<? y and y ?<? z implies x ?<? z.
class QuasiOrder a where
  infixl 4 ?<?
  (?<?) :: MonadPartial p => a -> a -> p ()

-- Flipped (?<?).
infixl 4 ?>?
(?>?) :: (MonadPartial p, QuasiOrder a) => a -> a -> p ()
(?>?) = flip (?<?)

instance PartialOrd a => QuasiOrder (TotalWrap a) where
  (TotalWrap x) ?<? (TotalWrap y) =
    if (x `leq` y) && (x /= y)
      then pure ()
      else empty

newtype TotalWrap a = TotalWrap
  { getTotalWrap :: a
  } deriving ( Eq
             , Ord
             , Show
             , JoinSemiLattice
             , MeetSemiLattice
             , Lattice
             , DistributiveLattice
             )

instance QuasiOrder () where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Bool where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Bool where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Ordering where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Ordering where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Char where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Char where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Int where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int8 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Int8 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int16 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Int16 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int32 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Int32 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int64 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Int64 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Word where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word8 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Word8 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word16 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Word16 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word32 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Word32 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word64 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Word64 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Integer where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Integer where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Natural where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance QuasiOrder Natural where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
