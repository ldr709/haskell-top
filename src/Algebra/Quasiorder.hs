{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Quasiorder where

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
class Quasiorder a where
  infixl 4 ?<?
  (?<?) :: MonadPartial p => a -> a -> p ()

-- Flipped (?<?).
infixl 4 ?>?
(?>?) :: (MonadPartial p, Quasiorder a) => a -> a -> p ()
(?>?) = flip (?<?)

instance PartialOrd a => Quasiorder (TotalWrap a) where
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

instance Quasiorder () where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Bool where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Bool where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Ordering where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Ordering where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Char where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Char where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Int where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int8 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Int8 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int16 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Int16 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int32 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Int32 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Int64 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Int64 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Word where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word8 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Word8 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word16 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Word16 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word32 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Word32 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Word64 where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Word64 where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Integer where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Integer where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
instance PartialOrd Natural where
  x `leq` y = (Ordered x) `leq` (Ordered y)
instance Quasiorder Natural where
  x ?<? y = (TotalWrap x) ?<? (TotalWrap y)
