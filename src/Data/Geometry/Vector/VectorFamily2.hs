{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily where

import           Control.Lens hiding (element)
import qualified Data.Foldable as F
import           Data.Geometry.Vector (Arity(..))
import qualified Data.Geometry.Vector.VectorFixed as FV
import           Data.Proxy
import           Data.Vector.Fixed.Cont (PeanoNum(..), Peano)
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector


type family VectorF (d :: Nat) where
  VectorF 1        = Identity
  VectorF 2        = L2.V2
  VectorF 3        = L3.V3
  VectorF 4        = L4.V4
  VectorF d        = FV.Vector d

newtype Vector (d :: Nat) (r :: *) = Vector { _unV :: VectorF d r }

unV :: Lens (Vector d r) (Vector d s) (VectorF d r) (VectorF d s)
unV = lens _unV (const Vector)




-- instance (Show r, Arity d) => Show (Vector d r) where
  -- shows v =



pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = (Vector (L2.V2 x y))

pattern Vector3        :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z  = (Vector (L3.V3 x y z))

pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = (Vector (L4.V4 x y z w))

class HasElement' (i :: Nat) (v :: * -> *) (b :: Bool) where
  elementL :: Arity i => proxy0 b -> proxy i -> Lens' (v r) r

instance HasElement' 0 Identity False where
  elementL _ _ = lens runIdentity (const Identity)
instance HasElement' 0 L2.V2 False where
  elementL _ _ = L2._x
instance HasElement' 1 L2.V2 False where
  elementL _ _ = L2._y
instance HasElement' 0 L3.V3 False where
  elementL _ _ = L3._x
instance HasElement' 1 L3.V3 False where
  elementL _ _ = L3._y
instance HasElement' 2 L3.V3 False where
  elementL _ _ = L3._z
instance HasElement' 0 L4.V4 False where
  elementL _ _ = L4._x
instance HasElement' 1 L4.V4 False where
  elementL _ _ = L4._y
instance HasElement' 2 L4.V4 False where
  elementL _ _ = L4._z
instance HasElement' 3 L4.V4 False where
  elementL _ _ = L4._w

instance (Arity d, (i+1) <= d) => HasElement' i (FV.Vector d) True where
  elementL _ = FV.element

class HasElement (i :: Nat) (d :: Nat) where
  elementL' :: (Arity i, Arity d, (i+1) <= d)
            => proxy0 d -> proxy i -> Lens' (VectorF d r) r

instance (v ~ VectorF d, b ~ (5 <=? d), HasElement' i v b) => HasElement i d where
  elementL' _ = elementL (Proxy :: Proxy (5 <=? d))


element   :: forall proxy i d r. (Arity i, Arity d, (i+1) <= d, HasElement i d)
          => proxy i -> Lens' (Vector d r) r
element i = unV.elementL' (Proxy :: Proxy d) i


-- vectorFromListUnsafe :: forall d r. Arity d => [r] -> Vector d r
-- vectorFromListUnsafe = \case
--     []             -> error "Data.Vector.VectorFamily.vectorFromListUnsafe: Empty vector"
--     [x] | expect 1 -> Vector (Identity x)

--   where
--     d = natVal (Proxy :: Proxy d)

--     expect l = d == l


-- class HasElement (i :: Nat) (d :: Nat) (b :: Bool) where
--   elementL :: (i <= d, Arity i, Arity d) => proxy0 b -> proxy i -> Lens' (Vector d r) r

-- instance HasElement 0 2 False where
--   elementL _ _ = unV.L2._x
-- instance HasElement 1 2 False where
--   elementL _ = unV.L2._y
-- instance HasElement 0 3 False where
--   elementL _ = unV.L3._x
-- instance HasElement 1 3 False where
--   elementL _ = unV.L3._y
-- instance HasElement 2 3 False where
--   elementL _ = unV.L3._z

-- instance HasElement i d True where
--   element _ px = unV.FV.element px
