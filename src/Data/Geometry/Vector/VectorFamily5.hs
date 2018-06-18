{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily5 where

import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Lens hiding (element)
import           Data.Aeson (ToJSON(..),FromJSON(..))
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


-- | The different implementation types we consider
data SelectD = Zero | One | Two | Three | Four | Many

-- | Mapping from natural numbers to the different implementation types
type family SelectF (d :: Nat) where
  SelectF 0        = Zero
  SelectF 1        = One
  SelectF 2        = Two
  SelectF 3        = Three
  SelectF 4        = Four
  SelectF d        = Many

-- | Mapping between the implementation type, and the actual implementation.
type family VectorFamilyF (s :: SelectD) (d :: Nat) = result | result -> s where
  VectorFamilyF Zero   d = Const ()
  VectorFamilyF One    d = Identity
  VectorFamilyF Two    d = L2.V2
  VectorFamilyF Three  d = L3.V3
  VectorFamilyF Four   d = L4.V4
  VectorFamilyF Many   d = FV.Vector d

-- | We select a vector depending on its length d
newtype VectorFamily (s :: SelectD) (d :: Nat) (r :: *) =
  VectorFamily { _unVF :: VectorFamilyF s d r }

unVF :: Lens (VectorFamily  s d r) (VectorFamily s d t)
             (VectorFamilyF s d r) (VectorFamilyF s d t)
unVF = lens _unVF (const VectorFamily)
{-# INLINE unVF #-}

deriving instance Functor (VectorFamily Zero d)
deriving instance Functor (VectorFamily One d)
deriving instance Functor (VectorFamily Two d)
deriving instance Functor (VectorFamily Three d)
deriving instance Functor (VectorFamily Four d)
deriving instance V.Arity d => Functor (VectorFamily Many d)

deriving instance Foldable (VectorFamily Zero d)
deriving instance Foldable (VectorFamily One d)
deriving instance Foldable (VectorFamily Two d)
deriving instance Foldable (VectorFamily Three d)
deriving instance Foldable (VectorFamily Four d)
deriving instance V.Arity d => Foldable (VectorFamily Many d)

deriving instance Traversable (VectorFamily Zero d)
deriving instance Traversable (VectorFamily One d)
deriving instance Traversable (VectorFamily Two d)
deriving instance Traversable (VectorFamily Three d)
deriving instance Traversable (VectorFamily Four d)
deriving instance V.Arity d => Traversable (VectorFamily Many d)

deriving instance Applicative (VectorFamily Zero d)
deriving instance Applicative (VectorFamily One d)
deriving instance Applicative (VectorFamily Two d)
deriving instance Applicative (VectorFamily Three d)
deriving instance Applicative (VectorFamily Four d)
deriving instance V.Arity d => Applicative (VectorFamily Many d)

deriving instance Eq r => Eq (VectorFamily Zero d r)
deriving instance Eq r => Eq (VectorFamily One d r)
deriving instance Eq r => Eq (VectorFamily Two d r)
deriving instance Eq r => Eq (VectorFamily Three d r)
deriving instance Eq r => Eq (VectorFamily Four d r)
deriving instance (V.Arity d, Eq r) => Eq (VectorFamily Many d r)

deriving instance Ord r => Ord (VectorFamily Zero d r)
deriving instance Ord r => Ord (VectorFamily One d r)
deriving instance Ord r => Ord (VectorFamily Two d r)
deriving instance Ord r => Ord (VectorFamily Three d r)
deriving instance Ord r => Ord (VectorFamily Four d r)
deriving instance (V.Arity d, Ord r) => Ord (VectorFamily Many d r)


deriving instance (NFData (VectorFamilyF s d r)) => NFData (VectorFamily s d r)


-- | Datatype representing d dimensional vectors. The default implementation is
-- based n VectorFixed. However, for small vectors we automatically select a
-- more efficient representation.
newtype Vector (d :: Nat) (r :: *) =
  MKVector { _unV :: VectorFamily (SelectF d) d r }

unV :: Lens (Vector d r)                   (Vector d t)
            (VectorFamily (SelectF d) d r) (VectorFamily (SelectF d) d t)
unV = lens _unV (const MKVector)
{-# INLINE unV #-}


-- | Class to select the particular vector implementation
class VectorSelect (s :: SelectD) where
  select :: f Zero -> f One -> f Two -> f Three -> f Four -> f Many -> f s
instance VectorSelect Zero where
  select f0 _ _ _ _ _ = f0
instance VectorSelect One where
  select _ f1 _ _ _ _ = f1
instance VectorSelect Two where
  select _ _ f2 _ _ _ = f2
instance VectorSelect Three where
  select _ _ _ f3 _ _ = f3
instance VectorSelect Four where
  select _ _ _ _ f4 _ = f4
instance VectorSelect Many where
  select _ _ _ _ _ fm = fm

-- | To be able to select based on d, you basically need this constraint
type Arity d = (VectorSelect (SelectF d), V.Arity d)

--------------------------------------------------------------------------------

newtype Compute f a b d s =
  Compute { run :: VectorFamily s d a -> f (VectorFamily s d b) }

withV   :: Functor f
        => Compute f a b d (SelectF d) -> Vector d a -> f (Vector d b)
withV c = fmap MKVector . run c . _unV


selectD     :: (Functor f, VectorSelect (SelectF d)
               , constr (VectorFamily Zero  d)
               , constr (VectorFamily One   d)
               , constr (VectorFamily Two   d)
               , constr (VectorFamily Three d)
               , constr (VectorFamily Four  d)
               , constr (VectorFamily Many  d)
               )
            => proxy constr
            -> (forall s. constr (VectorFamily s d) => Compute f a b d s)
            -> Vector d a -> f (Vector d b)
selectD _ c = withV (select c c c c c c)


--------------------------------------------------------------------------------

instance Arity d => Functor (Vector d) where
  fmap f = runIdentity . selectD (Proxy :: Proxy Functor) (mkFMap f)

mkFMap   :: Functor (VectorFamily s d)
         => (a -> b) -> Compute Identity a b d s
mkFMap f = Compute $ Identity . fmap f


--------------------------------------------------------------------------------

newtype Consume a b d s =
  Consume { runConsume :: VectorFamily s d a -> b }

withVC   :: Consume a b d (SelectF d) -> Vector d a -> b
withVC c = runConsume c . _unV

selectC     :: ( VectorSelect (SelectF d)
               , constr (VectorFamily Zero  d)
               , constr (VectorFamily One   d)
               , constr (VectorFamily Two   d)
               , constr (VectorFamily Three d)
               , constr (VectorFamily Four  d)
               , constr (VectorFamily Many  d)
               )
            => proxy constr
            -> (forall s. constr (VectorFamily s d) => Consume a b d s)
            -> Vector d a -> b
selectC _ c = withVC (select c c c c c c)

--------------------------------------------------------------------------------

mkFoldMap   :: (Foldable (VectorFamily s d), Monoid m)
            => (a -> m) -> Consume a m d s
mkFoldMap f = Consume $ foldMap f

instance Arity d => Foldable (Vector d) where
  foldMap f = selectC (Proxy :: Proxy Foldable) (mkFoldMap f)
  length _ = fromInteger $ natVal (Proxy :: Proxy d)
  null v = length v == 0

instance Arity d => Traversable (Vector d) where
  traverse f = selectD (Proxy :: Proxy Traversable) (mkTraverse f)

mkTraverse   :: (Applicative f, Traversable (VectorFamily s d))
             => (a -> f b) -> Compute f a b d s
mkTraverse f = Compute $ traverse f


--------------------------------------------------------------------------------

newtype Compute2 f a b c d s =
  Compute2 { run2 :: VectorFamily s d a -> VectorFamily s d b -> f (VectorFamily s d c) }

withV2                             :: Functor f
                                   => Compute2 f a b c d (SelectF d)
                                   -> Vector d a -> Vector d b -> f (Vector d c)
withV2 c (MKVector u) (MKVector v) = MKVector <$> run2 c u v

selectD2     :: (Functor f, VectorSelect (SelectF d)
               , constr (VectorFamily Zero  d)
               , constr (VectorFamily One   d)
               , constr (VectorFamily Two   d)
               , constr (VectorFamily Three d)
               , constr (VectorFamily Four  d)
               , constr (VectorFamily Many  d)
               )
            => proxy constr
            -> (forall s. constr (VectorFamily s d) => Compute2 f a b c d s)
            -> Vector d a -> Vector d b -> f (Vector d c)
selectD2 _ c = withV2 (select c c c c c c)

--------------------------------------------------------------------------------

instance Arity d => Applicative (Vector d) where
  pure         = runPure
  u <*> v      = runIdentity $ selectD2 (Proxy :: Proxy Applicative) mkApp        u v
  liftA2 f u v = runIdentity $ selectD2 (Proxy :: Proxy Applicative) (mkLiftA2 f) u v

newtype Vec r d s = Vec { runVec :: VectorFamily s d r }

runPure   :: forall d r. Arity d => r -> Vector d r
runPure x = MKVector . runVec $ select c c c c c c
  where
    c :: forall s. Applicative (VectorFamily s d) => Vec r d s
    c = Vec $ pure x

mkApp :: Applicative (VectorFamily s d) => Compute2 Identity (a -> b) a b d s
mkApp = Compute2 $ \fu v -> Identity $ fu <*> v

mkLiftA2   :: Applicative (VectorFamily s d)
           => (a -> b -> c) -> Compute2 Identity a b c d s
mkLiftA2 f = Compute2 $ \u v -> Identity $ liftA2 f u v

--------------------------------------------------------------------------------

instance (Arity d, Eq r) => Eq (Vector d r) where
  u == v = and $ liftA2 (==) u v
instance (Arity d, Ord r) => Ord (Vector d r) where
  u `compare` v = F.fold $ liftA2 compare u v

instance (Show r, Arity d) => Show (Vector d r) where
  show v = mconcat [ "Vector", show $ length v , " "
                   , show $ F.toList v
                   ]

deriving instance (NFData (VectorFamily (SelectF d) d r)) => NFData (Vector d r)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Convenience "constructors"

pattern Vector   :: VectorFamilyF (SelectF d) d r -> Vector d r
pattern Vector v = MKVector (VectorFamily v)
{-# COMPLETE Vector #-}

pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = (Vector (Identity x))

pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = (Vector (L2.V2 x y))

pattern Vector3        :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z  = (Vector (L3.V3 x y z))

pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = (Vector (L4.V4 x y z w))

--------------------------------------------------------------------------------

-- -- destruct            :: (Vec d r, Vec (d + 1) r, 1 <= (d + 1))
-- --                     => Vector (d + 1) r -> (r, Vector d r)
-- -- destruct (Vector v) = (V.head v, Vector $ V.tail v)


-- -- -- vectorFromList :: Arity d => [a] -> Maybe (Vector d a)
-- -- vectorFromList = fmap Vector . V.fromListM

-- -- vectorFromListUnsafe :: V.Arity d => [a] -> Vector d a
-- -- vectorFromListUnsafe = Vector . V.fromList

 --------------------------------------------------------------------------------

-- | Cross product of two three-dimensional vectors
cross :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
(Vector u) `cross` (Vector v) = Vector $ u `L3.cross` v
