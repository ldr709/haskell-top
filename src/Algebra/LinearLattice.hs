module Algebra.LinearLattice where

import Algebra.Apartness
import Algebra.Lattice
import Algebra.LinearOrder
import Algebra.PartialOrd
import Algebra.Quasiorder
import Algebra.SemiBoundedLattice
import Control.Applicative
import Control.Monad
import Control.Monad.Partial
import Data.Int
import Data.Word
import Numeric.Natural

-- Both a DistributiveLattice and a LinearOrder, with some laws for consistency.
--
-- Laws:
-- * Consistency: x \/ y ?/=? x if and only if x ?<? y
-- *              x /\ y ?/=? x if and only if y ?<? x
class (LinearOrder a, DistributiveLattice a) => LinearLattice a

instance (PartialOrd a, Ord a, DistributiveLattice a) =>
         LinearLattice (TotalWrap a)

instance DistributiveLattice ()
instance LinearLattice ()
instance DistributiveLattice Bool
instance LinearLattice Bool
instance MeetSemiLattice Ordering where
  (/\) = min
instance JoinSemiLattice Ordering where
  (\/) = max
instance Lattice Ordering
instance DistributiveLattice Ordering
instance LinearLattice Ordering
instance MeetSemiLattice Char where
  (/\) = min
instance JoinSemiLattice Char where
  (\/) = max
instance Lattice Char
instance DistributiveLattice Char
instance LinearLattice Char
instance MeetSemiLattice Int where
  (/\) = min
instance JoinSemiLattice Int where
  (\/) = max
instance Lattice Int
instance DistributiveLattice Int
instance LinearLattice Int
instance MeetSemiLattice Int8 where
  (/\) = min
instance JoinSemiLattice Int8 where
  (\/) = max
instance Lattice Int8
instance DistributiveLattice Int8
instance LinearLattice Int8
instance MeetSemiLattice Int16 where
  (/\) = min
instance JoinSemiLattice Int16 where
  (\/) = max
instance Lattice Int16
instance DistributiveLattice Int16
instance LinearLattice Int16
instance MeetSemiLattice Int32 where
  (/\) = min
instance JoinSemiLattice Int32 where
  (\/) = max
instance Lattice Int32
instance DistributiveLattice Int32
instance LinearLattice Int32
instance MeetSemiLattice Int64 where
  (/\) = min
instance JoinSemiLattice Int64 where
  (\/) = max
instance Lattice Int64
instance DistributiveLattice Int64
instance LinearLattice Int64
instance MeetSemiLattice Word where
  (/\) = min
instance JoinSemiLattice Word where
  (\/) = max
instance Lattice Word
instance DistributiveLattice Word
instance LinearLattice Word
instance MeetSemiLattice Word8 where
  (/\) = min
instance JoinSemiLattice Word8 where
  (\/) = max
instance Lattice Word8
instance DistributiveLattice Word8
instance LinearLattice Word8
instance MeetSemiLattice Word16 where
  (/\) = min
instance JoinSemiLattice Word16 where
  (\/) = max
instance Lattice Word16
instance DistributiveLattice Word16
instance LinearLattice Word16
instance MeetSemiLattice Word32 where
  (/\) = min
instance JoinSemiLattice Word32 where
  (\/) = max
instance Lattice Word32
instance DistributiveLattice Word32
instance LinearLattice Word32
instance MeetSemiLattice Word64 where
  (/\) = min
instance JoinSemiLattice Word64 where
  (\/) = max
instance Lattice Word64
instance DistributiveLattice Word64
instance LinearLattice Word64
instance Lattice Integer
instance DistributiveLattice Integer
instance LinearLattice Integer
instance MeetSemiLattice Natural where
  (/\) = min
instance JoinSemiLattice Natural where
  (\/) = max
instance Lattice Natural
instance DistributiveLattice Natural
instance LinearLattice Natural

