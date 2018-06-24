{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily8 where

import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Lens hiding (element)
import           Unsafe.Coerce (unsafeCoerce)
-- import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Foldable as F
import           Data.Functor.Contravariant
import qualified Data.Geometry.Vector.VectorFixed as FV
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Semigroup
import           Data.Traversable (foldMapDefault,fmapDefault)
import qualified Data.Vector.Fixed as V
import           Data.Vector.Fixed.Cont (Peano(..))
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector

--------------------------------------------------------------------------------
-- * d dimensional Vectors

type Arity d = V.Arity d

-- | Mapping between the implementation type, and the actual implementation.
type family VectorFamily (d :: Nat) = result | result -> d where
  VectorFamily 0 = Const ()
  VectorFamily 1 = Identity
  VectorFamily 2 = L2.V2
  VectorFamily 3 = L3.V3
  VectorFamily 4 = L4.V4
  VectorFamily d = FV.Vector d

-- | Datatype representing d dimensional vectors. The default implementation is
-- based n VectorFixed. However, for small vectors we automatically select a
-- more efficient representation.
newtype Vector (d :: Nat) (r :: *) = Vector { _unV :: VectorFamily d r }

unV :: Lens (Vector d r)       (Vector d t)
            (VectorFamily d r) (VectorFamily d t)
unV = lens _unV (const Vector)
{-# INLINE unV #-}

instance Arity d => Functor (Vector d) where
  fmap = fmap'

fmap'   :: forall a b d. Arity d => (a -> b) -> Vector d a -> Vector d b
fmap' f = case natVal (Proxy :: Proxy d) of
          0 -> unsafeCoerce
               (fmap f :: VectorFamily 0 a -> VectorFamily 0 b)
          1 -> unsafeCoerce
               (fmap f :: VectorFamily 1 a -> VectorFamily 1 b)
          2 -> unsafeCoerce
               (fmap f :: VectorFamily 2 a -> VectorFamily 2 b)
          3 -> unsafeCoerce
               (fmap f :: VectorFamily 3 a -> VectorFamily 3 b)
          4 -> unsafeCoerce
               (fmap f :: VectorFamily 4 a -> VectorFamily 4 b)
          _ -> unsafeCoerce
               (fmap f :: FV.Vector    d a -> FV.Vector d b)

instance Arity d => Foldable (Vector d) where
  {-# SPECIALIZE instance Foldable (Vector 2) #-}
  foldMap = foldMap'
  length _ = fromInteger $ natVal (Proxy :: Proxy d)
  null v = length v == 0

foldMap'   :: forall a m d. (Arity d, Monoid m) => (a -> m) -> Vector d a -> m
foldMap' f = g . _unV
  where
    g :: VectorFamily d a -> m
    g = case natVal (Proxy :: Proxy d) of
          0 -> unsafeCoerce
               (foldMap f :: VectorFamily 0 a -> m)
          1 -> unsafeCoerce
               (foldMap f :: VectorFamily 1 a -> m)
          2 -> unsafeCoerce
               (foldMap f :: VectorFamily 2 a -> m)
          3 -> unsafeCoerce
               (foldMap f :: VectorFamily 3 a -> m)
          4 -> unsafeCoerce
               (foldMap f :: VectorFamily 4 a -> m)
          _ -> unsafeCoerce
               (foldMap f :: FV.Vector    d a -> m)



instance Arity d => Traversable (Vector d) where
  {-# SPECIALIZE instance Traversable (Vector 2) #-}
  traverse = trav'



instance (Show r, Arity d) => Show (Vector d r) where
  show v = mconcat [ "Vector", show $ length v , " "
                   , show $ F.toList v
                   ]
instance Arity d => Applicative (Vector d) where
  {-# SPECIALIZE instance Applicative (Vector 2) #-}
  pure = pure'
  liftA2 = liftA2'

pure'     :: forall d r. Arity d => r -> Vector d r
pure' x = Vector $ case natVal (Proxy :: Proxy d) of
               0 -> unsafeCoerce (pure x :: VectorFamily 0 r)
               1 -> unsafeCoerce (pure x :: VectorFamily 1 r)
               2 -> unsafeCoerce (pure x :: VectorFamily 2 r)
               3 -> unsafeCoerce (pure x :: VectorFamily 3 r)
               4 -> unsafeCoerce (pure x :: VectorFamily 4 r)
               _ -> unsafeCoerce (pure x :: FV.Vector    d r)

liftA2'         :: forall a b c d. Arity d => (a -> b -> c) -> Vector d a -> Vector d b
                -> Vector d c
liftA2' f va vb = Vector $ g (_unV va) (_unV vb)
  where
    g :: VectorFamily d a -> VectorFamily d b -> VectorFamily d c
    g = case natVal (Proxy :: Proxy d) of
          0 -> unsafeCoerce
               (liftA2 f :: VectorFamily 0 a -> VectorFamily 0 b -> VectorFamily 0 c)
          1 -> unsafeCoerce
               (liftA2 f :: VectorFamily 1 a -> VectorFamily 1 b -> VectorFamily 1 c)
          2 -> unsafeCoerce
               (liftA2 f :: VectorFamily 2 a -> VectorFamily 2 b -> VectorFamily 2 c)
          3 -> unsafeCoerce
               (liftA2 f :: VectorFamily 3 a -> VectorFamily 3 b -> VectorFamily 3 c)
          4 -> unsafeCoerce
               (liftA2 f :: VectorFamily 4 a -> VectorFamily 4 b -> VectorFamily 4 c)
          _ -> unsafeCoerce
               (liftA2 f :: FV.Vector    d a -> FV.Vector d b    -> FV.Vector d c)


deriving instance (NFData (VectorFamily d r)) => NFData (Vector d r)

instance (Arity d, Eq r) => Eq (Vector d r) where
  u == v = and $ liftA2 (==) u v
-- instance (Arity d, Ord r) => Ord (Vector d r) where
--   u `compare` v = F.fold $ liftA2 compare u v

instance (Arity d, Ord a) => Ord (Vector d a) where
  {-# SPECIALIZE instance Ord a => Ord (Vector 2 a) #-}
  u `compare` v = f u v
    where
      f = case natVal (Proxy :: Proxy d) of
          0 -> unsafeCoerce
               (compare :: VectorFamily 0 a -> VectorFamily 0 a -> Ordering)
          1 -> unsafeCoerce
               (compare :: VectorFamily 1 a -> VectorFamily 1 a -> Ordering)
          2 -> unsafeCoerce
               (compare :: VectorFamily 2 a -> VectorFamily 2 a -> Ordering)
          3 -> unsafeCoerce
               (compare :: VectorFamily 3 a -> VectorFamily 3 a -> Ordering)
          4 -> unsafeCoerce
               (compare :: VectorFamily 4 a -> VectorFamily 4 a -> Ordering)
          _ -> unsafeCoerce
               (compare :: FV.Vector    d a -> FV.Vector d a    -> Ordering)

testV :: Vector 6 Int
testV = Vector $ FV.vectorFromListUnsafe [0,1,2,3,4,5]

    -- (<*>)    = selectD2 (Proxy :: Proxy Applicative) mkApp
  -- liftA2 f = selectD2 (Proxy :: Proxy Applicative) (mkLiftA2 f)



-- instance V.Arity d => Traversable (Vector d) where
--   traverse f = fmap Vector
--              . trav (traverse f) (traverse f) (traverse f)
--                     (traverse f) (traverse f) (traverse f)
--              . _unV

-- trav                     :: forall d f m r b. (KnownNat d, Applicative f)
--                             => (VectorFamily 0 r -> f (VectorFamily 0 b))
--                             -> (VectorFamily 1 r -> f (VectorFamily 1 b))
--                             -> (VectorFamily 2 r -> f (VectorFamily 2 b))
--                             -> (VectorFamily 3 r -> f (VectorFamily 3 b))
--                             -> (VectorFamily 4 r -> f (VectorFamily 4 b))
--                             -> (FV.Vector    m r -> f (FV.Vector    m b))
--                             -> VectorFamily d r -> f (VectorFamily d b)
-- trav f0 f1 f2 f3 f4 fm v = case natVal $ (Proxy :: Proxy d) of
--                      0 -> let v' :: VectorFamily 0 r = unsafeCoerce v
--                           in unsafeCoerce $ f0 v'
--                      1 -> let v' :: VectorFamily 1 r = unsafeCoerce v
--                           in unsafeCoerce $ f1 v'
--                      2 -> let v' :: VectorFamily 2 r = unsafeCoerce v
--                           in unsafeCoerce $ f2 v'
--                      3 -> let v' :: VectorFamily 3 r = unsafeCoerce v
--                           in unsafeCoerce $ f3 v'
--                      4 -> let v' :: VectorFamily 4 r = unsafeCoerce v
--                           in unsafeCoerce $ f4 v'
--                      _ -> let v' :: FV.Vector    m r = unsafeCoerce v
--                           in unsafeCoerce $ fm v'




trav'   :: forall a b d f. (V.Arity d, Applicative f)
        => (a -> f b) -> Vector d a -> f (Vector d b)
trav' f = fmap Vector . select (traverse f) . _unV
  where
    select     :: (forall t. Traversable t => t a -> f (t b))
               -> VectorFamily d a
               -> f (VectorFamily d b)
    select g v = case natVal $ (Proxy :: Proxy d) of
                     0 -> let v' :: VectorFamily 0 a = unsafeCoerce v
                          in unsafeCoerce $ g v'
                     1 -> let v' :: VectorFamily 1 a = unsafeCoerce v
                          in  unsafeCoerce $ g v'
                     2 -> let v' :: VectorFamily 2 a = unsafeCoerce v
                          in  unsafeCoerce $ g v'
                     3 -> let v' :: VectorFamily 3 a = unsafeCoerce v
                          in  unsafeCoerce $ g v'
                     4 -> let v' :: VectorFamily 4 a = unsafeCoerce v
                          in  unsafeCoerce $ g v'
                     _ -> let v' :: FV.Vector    d a = unsafeCoerce v
                          in  unsafeCoerce $ g v'



-- select'                     :: forall d f m r. KnownNat d
--                             => (VectorFamily 0 r -> f 0)
--                             -> (VectorFamily 1 r -> f 1)
--                             -> (VectorFamily 2 r -> f 2)
--                             -> (VectorFamily 3 r -> f 3)
--                             -> (VectorFamily 4 r -> f 4)
--                             -> (FV.Vector    m r -> f m)
--                             -> VectorFamily d r -> f d
-- select' f0 f1 f2 f3 f4 fm v = case natVal $ (Proxy :: Proxy d) of
--                      0 -> let v' :: VectorFamily 0 r = unsafeCoerce v
--                           in unsafeCoerce $ f0 v'
--                      1 -> let v' :: VectorFamily 1 r = unsafeCoerce v
--                           in unsafeCoerce $ f1 v'
--                      2 -> let v' :: VectorFamily 2 r = unsafeCoerce v
--                           in unsafeCoerce $ f2 v'
--                      3 -> let v' :: VectorFamily 3 r = unsafeCoerce v
--                           in unsafeCoerce $ f3 v'
--                      4 -> let v' :: VectorFamily 4 r = unsafeCoerce v
--                           in unsafeCoerce $ f4 v'
--                      _ -> let v' :: FV.Vector    m r = unsafeCoerce v
--                           in unsafeCoerce $ fm v'




-- withV   :: Functor f
--         => (VectorFamily d a -> )
--         f a b d -> Vector d a -> f (Vector d b)
-- withV c = fmap Vector . runVec . c . _unV



-- type Compute f a b d = VectorFamily d a -> Vec f b d

-- -- newtype Compute f a b d =
-- --   Compute { run :: VectorFamily d a -> f (VectorFamily d b) }



-- mkFMap   :: Functor (VectorFamily d)
--          => (a -> b) -> Compute Identity a b d
-- mkFMap f = Vec . Identity . fmap f


-- instance V.Arity d => Functor (Vector d) where
--   fmap f = withV (select' c c c c c)
--     where
--       c = mkFMap f

-- select     :: (Functor f
--                , constr (VectorFamily 0)
--                , constr (VectorFamily 1)
--                , constr (VectorFamily 2)
--                , constr (VectorFamily 3)
--                , constr (VectorFamily 4)
--                , constr (FV.Vector    m)
--                )
--               => proxy constr
--              -> (forall d'. constr (VectorFamily d') => Compute f a b d')
--              -> (forall d'. constr (FV.Vector m) => Compute f a b m)
--              -> Vector d a -> f (Vector d b)
-- select _ c m = withV (select' c c c c c m)



-- instance V.Arity d => Functor (Vector d) where
--   fmap = fmap'

-- fmap'   :: forall a b d. V.Arity d => (a -> b) -> Vector d a -> Vector d b
-- fmap' f = Vector . select (fmap f) . _unV
--     where
--       select     :: (forall f. Functor f => (f a -> f b)) -> VectorFamily d a -> VectorFamily d b
--       select g v = case natVal $ (Proxy :: Proxy d) of
--                      0 -> let v' :: VectorFamily 0 a = unsafeCoerce v
--                           in unsafeCoerce $ g v'
--                      1 -> let v' :: VectorFamily 1 a = unsafeCoerce v
--                           in  unsafeCoerce $ g v'
--                      2 -> let v' :: VectorFamily 2 a = unsafeCoerce v
--                           in  unsafeCoerce $ g v'
--                      3 -> let v' :: VectorFamily 3 a = unsafeCoerce v
--                           in  unsafeCoerce $ g v'
--                      4 -> let v' :: VectorFamily 4 a = unsafeCoerce v
--                           in  unsafeCoerce $ g v'
--                      _ -> let v' :: FV.Vector    d a = unsafeCoerce v
--                           in  unsafeCoerce $ g v'




instance Arity d => Additive (Vector d) where
  zero = pure 0
  u ^+^ v = liftA2 (+) u v

instance Arity d => Affine (Vector d) where
  type Diff (Vector d) = Vector d
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v

instance Arity d => Metric (Vector d)

--------------------------------------------------------------------------------

type instance V.Dim (Vector d)  = d


type instance Index   (Vector d r) = Int
type instance IxValue (Vector d r) = r


--------------------------------------------------------------------------------
-- * Convenience "constructors"

pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = (Vector (Identity x))

pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = (Vector (L2.V2 x y))

pattern Vector3        :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z  = (Vector (L3.V3 x y z))

pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = (Vector (L4.V4 x y z w))
