{-# language BangPatterns #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}

module Fancy
  ( Vec(..)
  , Optimal
  , dot
    -- * Construction
  , fromVector
  , singleton
  , doubleton
  , triple
  , quadruple
  , fromP
  ) where

import Nat (Nat(..),SingNat(..),ImplicitNat(..),N1,N2,N3,N4)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import           Control.DeepSeq
import Linear.V0 (V0(..))
import Linear.V1 (V1(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

import qualified Simple
import qualified Vector as V
import qualified Linear.Metric

import Data.Geometry.Point
import Data.Ext



type family Optimal (n :: Nat) :: Type -> Type where
  Optimal 'Zero = V0
  Optimal ('Succ 'Zero) = V1
  Optimal ('Succ ('Succ 'Zero)) = V2
  Optimal ('Succ ('Succ ('Succ 'Zero))) = V3
  Optimal ('Succ ('Succ ('Succ ('Succ 'Zero)))) = V4
  Optimal ('Succ ('Succ ('Succ ('Succ ('Succ m))))) =
    Simple.Vec ('Succ ('Succ ('Succ ('Succ ('Succ m)))))

newtype Vec n a = Vec (Optimal n a)

deriving instance NFData (Optimal n r) => NFData (Vec n r)

instance (ImplicitNat n, Eq a) => Eq (Vec n a) where
  Vec x == Vec y = case (implicitNat :: SingNat n) of
    SingZero -> x == y
    SingSucc SingZero -> x == y
    SingSucc (SingSucc SingZero) -> x == y
    SingSucc (SingSucc (SingSucc SingZero)) -> x == y
    SingSucc (SingSucc (SingSucc (SingSucc SingZero))) -> x == y
    SingSucc (SingSucc (SingSucc (SingSucc (SingSucc _)))) -> x == y

instance (ImplicitNat n, Ord a) => Ord (Vec n a) where
  compare (Vec x) (Vec y) = case (implicitNat :: SingNat n) of
    SingZero -> compare x y
    SingSucc SingZero -> compare x y
    SingSucc (SingSucc SingZero) -> compare x y
    SingSucc (SingSucc (SingSucc SingZero)) -> compare x y
    SingSucc (SingSucc (SingSucc (SingSucc SingZero))) -> compare x y
    SingSucc (SingSucc (SingSucc (SingSucc (SingSucc _)))) -> compare x y
  {-# INLINE compare #-}

fromVector :: V.Vector n a -> Vec n a
fromVector = \case
  V.VectorNil -> Vec V0
  V.VectorCons a V.VectorNil -> Vec (V1 a)
  V.VectorCons a (V.VectorCons b V.VectorNil) -> Vec (V2 a b)
  V.VectorCons a (V.VectorCons b (V.VectorCons c V.VectorNil)) -> Vec (V3 a b c)
  V.VectorCons a (V.VectorCons b (V.VectorCons c (V.VectorCons d V.VectorNil))) -> Vec (V4 a b c d)
  v@(V.VectorCons _ (V.VectorCons _ (V.VectorCons _ (V.VectorCons _ (V.VectorCons _ V.VectorNil))))) -> Vec (Simple.fromVector v)

singleton :: a -> Vec N1 a
singleton a = Vec (V1 a)

doubleton :: a -> a -> Vec N2 a
doubleton a b = Vec (V2 a b)

triple :: a -> a -> a -> Vec N3 a
triple a b c = Vec (V3 a b c)

quadruple :: a -> a -> a -> a -> Vec N4 a
quadruple a b c d = Vec (V4 a b c d)

dot :: forall n a. (Num a, ImplicitNat n) => Vec n a -> Vec n a -> a
dot (Vec x) (Vec y) = case (implicitNat :: SingNat n) of
  SingZero -> Linear.Metric.dot x y
  SingSucc SingZero -> Linear.Metric.dot x y
  SingSucc (SingSucc SingZero) -> Linear.Metric.dot x y
  SingSucc (SingSucc (SingSucc SingZero)) -> Linear.Metric.dot x y
  SingSucc (SingSucc (SingSucc (SingSucc SingZero))) -> Linear.Metric.dot x y
  SingSucc (SingSucc (SingSucc (SingSucc (SingSucc _)))) -> Simple.dot x y

fromP :: Point 2 r :+ e -> Vec N2 r :+ e
fromP (Point2 x y :+ e) = doubleton x y :+ e
