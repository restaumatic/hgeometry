{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Simple
  ( Vec
  , dot
    -- * Construction
  , fromVector
  , singleton
  , doubleton
  , triple
  , quadruple
  ) where

import Nat
import Vector (Vector)
import Data.Primitive.SmallArray
import Control.Monad.ST (ST,runST)

import qualified Vector as V

newtype Vec (n :: Nat) a = Vec (SmallArray a)
  deriving (Functor,Eq,Ord)

-- | Dot product of two vectors
dot :: Num a => Vec n a -> Vec n a -> a
dot (Vec xs) (Vec ys) = undefined
  -- let len = sizeofSmallArray xs
  --     go !acc ix = if ix < len
  --       then
  --         let (# x #) = indexSmallArray## xs ix
  --             (# y #) = indexSmallArray## ys ix
  --          in go (x * y + acc) (ix + 1)
  --       else acc
  --  in go 0 0

fromVector :: forall n a. Vector n a -> Vec n a
fromVector v0 = Vec $ runST action where
  action :: forall s. ST s (SmallArray a)
  action = do
    m <- newSmallArray (V.length v0) unitializedElement
    let go :: forall m. Int -> Vector m a -> ST s ()
        go !_ V.VectorNil = return ()
        go !ix (V.VectorCons x xs) = do
          writeSmallArray m ix x
          go (ix + 1) xs
    go 0 v0
    unsafeFreezeSmallArray m

singleton :: a -> Vec N1 a
singleton a = Vec $ runST $ do
  newSmallArray 1 a >>= unsafeFreezeSmallArray

doubleton :: a -> a -> Vec N2 a
doubleton a b = Vec $ runST $ do
  m <- newSmallArray 2 a
  writeSmallArray m 1 b
  unsafeFreezeSmallArray m

triple :: a -> a -> a -> Vec N3 a
triple a b c = Vec $ runST $ do
  m <- newSmallArray 3 a
  writeSmallArray m 1 b
  writeSmallArray m 2 c
  unsafeFreezeSmallArray m

quadruple :: a -> a -> a -> a -> Vec N4 a
quadruple a b c d = Vec $ runST $ do
  m <- newSmallArray 4 a
  writeSmallArray m 1 b
  writeSmallArray m 2 c
  writeSmallArray m 3 d
  unsafeFreezeSmallArray m

{-# NOINLINE unitializedElement #-}
unitializedElement :: a
unitializedElement = error "Simple.unitializedElement"
