{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Algebra.Field where

-- Not all instances of Fractional are fields, so use this to mark ones as
-- actually obeying the field laws.
class Fractional a => Field a

instance Field Rational
