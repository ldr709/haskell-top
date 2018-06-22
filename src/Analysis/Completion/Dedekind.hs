{-# LANGUAGE RankNTypes #-}

module Analysis.Completion.Dedekind
  ( Dedekind()
  , dedekind
  , below
  , above
  , fromInner
  ) where

import Algebra.LinearOrder
import Algebra.QuasiOrder
import Control.Monad.Partial
import Topology.Compact

-- Dedekind completion of a. Definable opens given by the (quasi) order topology
-- inherited from a. Subbase is consists of (?<? x) and (?>? x), for all x.
data Dedekind a =
  Dedekind !(forall p. MonadPartial p => a -> p ())
           !(forall p. MonadPartial p => a -> p ())

-- Requirements:
-- l is less than u:     for all x in l and y in u, x < y.
-- l and u are nonempty.
-- l is downward closed: if s ?<? t then t in l implies s in l.
-- u is upward closed:   if s ?<? t then s in u implies t in u.
-- l is upwards open:    every finite subset of l has a strict upper bound in l.
-- r is downward open:   every finite subset of u has a strict lower bound in u.
-- TODO: If all l < x < all u, and all l < y < all u, then x = y.
dedekind ::
     (QuasiOrder a, Overt a)
  => (forall p. MonadPartial p => a -> p ())
  -> (forall p. MonadPartial p => a -> p ())
  -> Dedekind a
dedekind l u = Dedekind l u

below :: (MonadPartial p, QuasiOrder a, Overt a) => Dedekind a -> a -> p ()
below (Dedekind x y) = x

above :: (MonadPartial p, QuasiOrder a, Overt a) => Dedekind a -> a -> p ()
above (Dedekind x y) = y

fromInner :: (QuasiOrder a, Overt a) => a -> Dedekind a
fromInner x = dedekind (?<? x) (?>? x)

--instance Apartness (Dedekind a) where
--  x ?/=? y = (below x)

--instance QuasiOrder (Dedekind a) where
--  (?<?)
