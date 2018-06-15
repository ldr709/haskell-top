{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Top.Compact where

import Control.Applicative
import Control.Monad
import Control.Monad.Partial
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Plus
import Data.Int
import Data.List
import Data.Stream.Infinite
import Data.Universe
import Data.Void
import Data.Word
import Numeric.Natural

class Compact a where
  -- Witness that a is compact. Must only return when given an open set that
  -- contains the whole space.
  compact :: MonadPartial p => ContT () p a

class Overt a where
  -- Witness that a is overt. Must only return when given an open set that
  -- contains a point of the space. If there are multiple a's for which the
  -- partial function returns then one of them will be picked (probably
  -- nondeterministically).
  overt :: MonadPartial p => ContT b p a

-- Binary Tychonoff's theorem
instance (Compact a, Compact b) => Compact (a, b) where
  compact = liftA2 (,) compact compact
instance (Overt a, Overt b) => Overt (a, b) where
  overt = liftA2 (,) overt overt

-- Countable Tychonoff's theorem
instance Compact a => Compact (Stream a) where
  compact = liftA2 (:>) compact compact
instance Overt a => Overt (Stream a) where
  overt = liftA2 (:>) overt overt

instance Overt a => Overt [a] where
  overt = return [] <|> liftA2 (:) overt overt

instance (Compact a, Compact b) => Compact (Either a b) where
  compact = (Left <$> compact) `contSeq` (Right <$> compact)
instance (Overt a, Overt b) => Overt (Either a b) where
  overt = Left <$> overt <|> Right <$> overt

instance Compact Void where
  compact = ContT $ \_ -> return ()
instance Overt Void where
  overt = empty

instance Compact () where
  compact = return ()
instance Overt () where
  overt = return ()

instance Compact Bool where
  compact = return False `contSeq` return True
instance Overt Bool where
  overt = return False <|> return True

newtype CompactFinite a = CompactFinite { runCompactFinite :: a }
  deriving (Enum, Bounded, Universe, Finite, Show, Eq)

-- Finite -> Compact
-- TODO: Maybe use foldl'
instance Finite a => Compact (CompactFinite a) where
  compact = foldr (contSeq . pure) (ContT $ \_ -> return ()) universeF

-- Recursively Enumerable -> Overt
instance Universe a => Overt (CompactFinite a) where
  overt = foldr ((<|>) . pure) empty universe

-- <$> can be used to map compactness (and overtness) through any surjection.
instance Compact Ordering where
  compact = runCompactFinite <$> compact
instance Overt Ordering where
  overt = runCompactFinite <$> overt
instance Compact Char where
  compact = runCompactFinite <$> compact
instance Overt Char where
  overt = runCompactFinite <$> overt
instance Compact Int where
  compact = runCompactFinite <$> compact
instance Overt Int where
  overt = runCompactFinite <$> overt
instance Compact Int8 where
  compact = runCompactFinite <$> compact
instance Overt Int8 where
  overt = runCompactFinite <$> overt
instance Compact Int16 where
  compact = runCompactFinite <$> compact
instance Overt Int16 where
  overt = runCompactFinite <$> overt
instance Compact Int32 where
  compact = runCompactFinite <$> compact
instance Overt Int32 where
  overt = runCompactFinite <$> overt
instance Compact Int64 where -- Not very compact
  compact = runCompactFinite <$> compact
instance Overt Int64 where
  overt = runCompactFinite <$> overt
instance Compact Word where
  compact = runCompactFinite <$> compact
instance Overt Word where
  overt = runCompactFinite <$> overt
instance Compact Word8 where
  compact = runCompactFinite <$> compact
instance Overt Word8 where
  overt = runCompactFinite <$> overt
instance Compact Word16 where
  compact = runCompactFinite <$> compact
instance Overt Word16 where
  overt = runCompactFinite <$> overt
instance Compact Word32 where
  compact = runCompactFinite <$> compact
instance Overt Word32 where
  overt = runCompactFinite <$> overt
instance Compact Word64 where -- Not very compact
  compact = runCompactFinite <$> compact
instance Overt Word64 where
  overt = runCompactFinite <$> overt

instance Overt Integer where
  overt = runCompactFinite <$> overt
