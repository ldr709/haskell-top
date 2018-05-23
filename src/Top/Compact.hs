{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Top.Compact where

import Control.Applicative
import Control.Monad
import Control.Monad.Partial
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Plus
import Data.Int
import Data.Stream.Infinite
import Data.Void
import Data.Word
import Numeric.Natural

class Compact a where
  -- Witness that a is compact. Must only return when given an open set that
  -- contains the whole space.
  compact :: ContT () Partial a

class Overt a where
  -- Witness that a is overt. Must only return when given an open set that
  -- contains a point of the space.
  overt :: ContT () Partial a

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
  compact = ContT $ \o -> return ()
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

newtype CompactEnum a = CompactEnum { runCompactEnum :: a }
  deriving (Enum, Bounded, Show, Eq)

-- Finite -> Compact
instance (Enum a, Bounded a, Eq a) => Compact (CompactEnum a) where
  compact = go minBound
    where
      go x | x == maxBound = return maxBound
      go x | otherwise     = return x <|> go (succ x)

-- Discrete -> Overt
instance Enum a => Overt (CompactEnum a) where
  overt = nonneg start <|> neg start
    where
      nonneg x = return x <|> nonneg (succ x)
      neg x = return (pred x) <|> nonneg (pred x)
      start = toEnum 0

-- <$> can be used to map compactness (and overtness) through any surjection.
instance Compact Ordering where
  compact = runCompactEnum <$> compact
instance Overt Ordering where
  overt = runCompactEnum <$> overt
instance Compact Char where
  compact = runCompactEnum <$> compact
instance Overt Char where
  overt = runCompactEnum <$> overt
instance Compact Int where
  compact = runCompactEnum <$> compact
instance Overt Int where
  overt = runCompactEnum <$> overt
instance Compact Int8 where
  compact = runCompactEnum <$> compact
instance Overt Int8 where
  overt = runCompactEnum <$> overt
instance Compact Int16 where
  compact = runCompactEnum <$> compact
instance Overt Int16 where
  overt = runCompactEnum <$> overt
instance Compact Int32 where
  compact = runCompactEnum <$> compact
instance Overt Int32 where
  overt = runCompactEnum <$> overt
instance Compact Int64 where -- Not very compact
  compact = runCompactEnum <$> compact
instance Overt Int64 where
  overt = runCompactEnum <$> overt
instance Compact Word where
  compact = runCompactEnum <$> compact
instance Overt Word where
  overt = runCompactEnum <$> overt
instance Compact Word8 where
  compact = runCompactEnum <$> compact
instance Overt Word8 where
  overt = runCompactEnum <$> overt
instance Compact Word16 where
  compact = runCompactEnum <$> compact
instance Overt Word16 where
  overt = runCompactEnum <$> overt
instance Compact Word32 where
  compact = runCompactEnum <$> compact
instance Overt Word32 where
  overt = runCompactEnum <$> overt
instance Compact Word64 where -- Not very compact
  compact = runCompactEnum <$> compact
instance Overt Word64 where
  overt = runCompactEnum <$> overt
