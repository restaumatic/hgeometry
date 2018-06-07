{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily where

import           Control.Lens hiding (element)
import qualified Data.Foldable as F
import qualified Data.Vector.Fixed as V
import           Data.Geometry.Vector (Arity(..))
import qualified Data.Geometry.Vector.VectorFixed as FV
import           Data.Proxy
import           Data.Semigroup
import           Data.Vector.Fixed.Cont (PeanoNum(..), Peano)
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector



data Vector (d :: Nat) (r :: *) where
  V1 :: !r                   -> Vector 1     r
  V2 :: !(L2.V2 r)           -> Vector 2     r
  V3 :: !(L3.V3 r)           -> Vector 3     r
  V4 :: !(L4.V4 r)           -> Vector 4     r
  VD :: !(FV.Vector (5+d) r) -> Vector (5+d) r

instance Arity d => Functor (Vector d) where
  fmap f = \case
    (V1 r) -> V1 $ f r
    (V2 v) -> V2 $ fmap f v
    (V3 v) -> V3 $ fmap f v
    (V4 v) -> V4 $ fmap f v
    (VD v) -> VD $ fmap f v

instance Arity d => Foldable (Vector d) where
  foldMap f = \case
    (V1 r) -> f r
    (V2 v) -> foldMap f v
    (V3 v) -> foldMap f v
    (V4 v) -> foldMap f v
    (VD v) -> foldMap f v
  length = \case
    (V1 _) -> 1
    (V2 _) -> 2
    (V3 _) -> 3
    (V4 _) -> 4
    (VD v) -> length v

-- deriving instance (Arity d, NFData r) => NFData (Vector d r)

instance Arity d => Traversable (Vector d) where
  traverse f = \case
    (V1 r) -> V1 <$> f r
    (V2 v) -> V2 <$> traverse f v
    (V3 v) -> V3 <$> traverse f v
    (V4 v) -> V4 <$> traverse f v
    (VD v) -> VD <$> traverse f v

instance (Arity d, Eq r) => Eq (Vector d r) where
  v == w = F.toList v == F.toList w

instance (Arity d, Ord r) => Ord (Vector d r) where
  v `compare` w = F.toList v `compare` F.toList w

instance (Arity d, Show r) => Show (Vector d r) where
  show v = mconcat [ "Vector", show $ F.length v , " "
                   , show $ F.toList v ]

instance {-# OVERLAPPABLE #-} Applicative (Vector 1) where
  pure = V1
  (V1 f) <*> (V1 x) = V1 $ f x
instance {-# OVERLAPPABLE #-} Applicative (Vector 2) where
  pure x = V2 $ pure x
  (V2 f) <*> (V2 x) = V2 $ f <*> x
instance {-# OVERLAPPABLE #-} Applicative (Vector 3) where
  pure x = V3 $ pure x
  (V3 f) <*> (V3 x) = V3 $ f <*> x
instance {-# OVERLAPPABLE #-} Applicative (Vector 4) where
  pure x = V4 $ pure x
  (V4 f) <*> (V4 x) = V4 $ f <*> x
instance {-# OVERLAPPABLE #-} (Arity d, d ~ (5 + d0)) => Applicative (Vector d) where
  pure x = VD $ pure x
  (VD f) <*> (VD x) = VD $ f <*> x

instance  {-# OVERLAPPABLE #-} Additive (Vector 1) where
  zero = pure 0
  v ^+^ w = (+) <$> v <*> w
instance  {-# OVERLAPPABLE #-} Additive (Vector 2) where
  zero = pure 0
  v ^+^ w = (+) <$> v <*> w
instance  {-# OVERLAPPABLE #-} Additive (Vector 3) where
  zero = pure 0
  v ^+^ w = (+) <$> v <*> w
instance  {-# OVERLAPPABLE #-} Additive (Vector 4) where
  zero = pure 0
  v ^+^ w = (+) <$> v <*> w
instance  {-# OVERLAPPABLE #-} (Arity d, d ~ (5+d0)) => Additive (Vector d) where
  zero = pure 0
  v ^+^ w = (+) <$> v <*> w

instance  {-# OVERLAPPABLE #-} Affine (Vector 1) where
  type Diff (Vector 1) = Vector 1
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v
instance  {-# OVERLAPPABLE #-} Affine (Vector 2) where
  type Diff (Vector 2) = Vector 2
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v
instance  {-# OVERLAPPABLE #-} Affine (Vector 3) where
  type Diff (Vector 3) = Vector 3
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v
instance  {-# OVERLAPPABLE #-} Affine (Vector 4) where
  type Diff (Vector 4) = Vector 4
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v
instance  {-# OVERLAPPABLE #-} (Arity d, d ~ (5+d0)) => Affine (Vector d) where
  type Diff (Vector d) = Vector d
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v

instance  {-# OVERLAPPABLE #-} Metric (Vector 1)
instance  {-# OVERLAPPABLE #-} Metric (Vector 2)
instance  {-# OVERLAPPABLE #-} Metric (Vector 3)
instance  {-# OVERLAPPABLE #-} Metric (Vector 4)
instance  {-# OVERLAPPABLE #-} (Arity d, d ~ (5+d0)) => Metric (Vector d)

type instance V.Dim (Vector d) = d

instance V.Vector (Vector 1) r where
  construct = V.Fun V1
  inspect (V1 x) (V.Fun f) = f x
  basicIndex (V1 x) = \case
                        0 -> x
                        _ -> error "BasicIndex Vector 1: Index out of bounds"
instance V.Vector (Vector 2) r where
  construct = V.Fun Vector2
  inspect (Vector2 x y) (V.Fun f) = f x y
  basicIndex (Vector2 x y) = \case
                        0 -> x
                        1 -> y
                        _ -> error "BasicIndex Vector2: Index out of bounds"
instance V.Vector (Vector 3) r where
  construct = V.Fun Vector3
  inspect (Vector3 x y z) (V.Fun f) = f x y z
  basicIndex (Vector3 x y z) = \case
                        0 -> x
                        1 -> y
                        2 -> z
                        _ -> error "BasicIndex Vector2: Index out of bounds"
instance V.Vector (Vector 4) r where
  construct = V.Fun Vector4
  inspect (Vector4 x y z w) (V.Fun f) = f x y z w
  basicIndex (Vector4 x y z w) = \case
                        0 -> x
                        1 -> y
                        2 -> z
                        3 -> w
                        _ -> error "BasicIndex Vector2: Index out of bounds"

instance (Arity d, d ~ (5 + d0)) => V.Vector (Vector d) r where
  construct = VD <$> V.construct
  inspect (VD v) = V.inspect v
  basicIndex (VD v) = V.basicIndex v

element   :: (Arity i, Arity d, (i+1) <= d) => proxy i -> Lens' (Vector d r) r
element i = singular $ ix (fromIntegral $ natVal i)

type instance Index   (Vector d r) = Int
type instance IxValue (Vector d r) = r

instance Arity d => Ixed (Vector d r) where
  ix i f v' = case v' of
    (V1 x) -> case i of
                0 -> V1 <$> f x
                _ -> pure v'
    (V2 v) -> case i of
                0 -> V2 <$> L2._x f v
                1 -> V2 <$> L2._y f v
                _ -> pure v'
    (V3 v) -> case i of
                0 -> V3 <$> L3._x f v
                1 -> V3 <$> L3._y f v
                2 -> V3 <$> L3._z f v
                _ -> pure v'
    (V4 v) -> case i of
                0 -> V4 <$> L4._x f v
                1 -> V4 <$> L4._y f v
                2 -> V4 <$> L4._z f v
                3 -> V4 <$> L4._w f v
                _ -> pure v'
    (VD v) -> VD <$> FV.element' i f v


--------------------------------------------------------------------------------


vectorFromList :: Arity d => [a] -> Maybe (Vector d a)
vectorFromList = V.fromListM

vectorFromListUnsafe :: Arity d => [a] -> Vector d a
vectorFromListUnsafe = fromJust . vectorFromList


--------------------------------------------------------------------------------

pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = V2 (L2.V2 x y)

pattern Vector3       :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z = V3 (L3.V3 x y z)

pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y w z = V4 (L4.V4 x y z w)


--------------------------------------------------------------------------------

-- | Cross product of two three-dimensional vectors
cross :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
(V3 u) `cross` (V3 v) = V3 $ u `L3.cross` v
