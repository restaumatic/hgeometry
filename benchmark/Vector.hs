{-# language BangPatterns #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module Vector
  ( Vector(..)
  , length
  ) where

import Prelude hiding (length)
import Data.Kind (Type)
import Nat (Nat(Succ,Zero))

data Vector :: Nat -> Type -> Type where
  VectorNil :: Vector 'Zero a
  VectorCons :: a -> Vector n a -> Vector ('Succ n) a

length :: Vector n a -> Int
length = go 0 where
  go :: Int -> Vector m b -> Int
  go !n VectorNil = n
  go !n (VectorCons _ xs) = go (n + 1) xs

