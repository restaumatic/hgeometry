{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily where

import           Control.DeepSeq
import           Control.Lens hiding (element)
import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Foldable as F
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

-- | Datatype representing d dimensional vectors. The default implementation is
-- based n VectorFixed. However, for small vectors we automatically select a
-- more efficient representation.
newtype Vector (d :: Nat) (r :: *) = Vector { _unV :: VectorF d r }

-- | We select a vector depending on its length d
type VectorF (d :: Nat) = VectorFamily (SelectF d) d

unV :: Lens (Vector d r) (Vector d t)
            (VectorFamily (SelectF d) d r) (VectorFamily (SelectF d) d t)
unV = lens (\(Vector v) -> v) (const Vector)

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
type family VectorFamily (s :: SelectD) (d :: Nat) where
  VectorFamily Zero   d = Const ()
  VectorFamily One    d = Identity
  VectorFamily Two    d = L2.V2
  VectorFamily Three  d = L3.V3
  VectorFamily Four   d = L4.V4
  VectorFamily Many   d = FV.Vector d

-- | Class to select the particular vector implementation
class VectorSelect (s :: SelectD) where
  selectS :: proxy s
          -> f Zero
          -> f One
          -> f Two
          -> f Three
          -> f Four
          -> f Many
          -> f s

-- | Shortcut for selecting an implementation based on the type of d
select   :: forall proxy d f. Arity d
         => proxy d
         -> f Zero
         -> f One
         -> f Two
         -> f Three
         -> f Four
         -> f Many
         -> f (SelectF d)
select _ = selectS (Proxy :: Proxy (SelectF d))

instance VectorSelect Zero where
  selectS _ f0 _ _ _ _ _ = f0
instance VectorSelect One where
  selectS _ _ f1 _ _ _ _ = f1
instance VectorSelect Two where
  selectS _ _ _ f2 _ _ _ = f2
instance VectorSelect Three where
  selectS _ _ _ _ f3 _ _ = f3
instance VectorSelect Four where
  selectS _ _ _ _ _ f4 _ = f4
instance VectorSelect Many where
  selectS _ _ _ _ _ _ fm = fm


-- | To be able to select based on d, you basically need this constraint
type Arity d = (VectorSelect (SelectF d), V.Arity d)

--------------------------------------------------------------------------------

instance Arity d => Functor (Vector d) where
  fmap f =  Vector . go . _unV
    where
      d  = Proxy :: Proxy d
      go = runVF d $ select d (VecF $ fmap f)
                              (VecF $ fmap f)
                              (VecF $ fmap f)
                              (VecF $ fmap f)
                              (VecF $ fmap f)
                              (VecF $ fmap f)

newtype VecF d a b (s :: SelectD) =
  VecF { runVF' :: VectorFamily s d a -> VectorFamily s d b}
runVF   :: proxy d -> VecF d a b s -> VectorFamily s d a -> VectorFamily s d b
runVF _ = runVF'

instance Arity d => Foldable (Vector d) where
  foldMap = foldMapDefault
  length _ = fromInteger $ natVal (Proxy :: Proxy d)
  null v = length v == 0

instance Arity d => Traversable (Vector d) where
  traverse f = fmap Vector . go . _unV
    where
      d  = Proxy :: Proxy d
      go = runVT  d $ select d (VecT $ traverse f)
                               (VecT $ traverse f)
                               (VecT $ traverse f)
                               (VecT $ traverse f)
                               (VecT $ traverse f)
                               (VecT $ traverse f)

newtype VecT d a b f (s :: SelectD) = VecT (VectorFamily s d a -> f (VectorFamily s d b))

runVT            :: proxy d -> VecT d a b f s
                 -> VectorFamily s d a -> f (VectorFamily s d b)
runVT _ (VecT f) = f

--------------------------------------------------------------------------------

instance Arity d => Applicative (Vector d) where
  pure x = Vector . runVec d $ select d (p x) (p x) (p x) (p x) (p x) (p x)
    where
      d = Proxy :: Proxy d
      p :: (Applicative (VectorFamily s d)) => a -> Vec d a s
      p = Vec . pure

  g <*> u = Vector $ runApply d (select d a a a a a a) g u
    where
      d  = Proxy :: Proxy d
      a :: (Applicative (VectorFamily s d)) => Apply d a b s
      a = Apply $ \(Vector f) (Vector v) -> f <*> v

newtype Vec d r s = Vec (VectorFamily s d r)
runVec            :: proxy d -> Vec d r s -> VectorFamily s d r
runVec _ (Vec v) = v


newtype Apply d a b s =
  Apply (s ~ SelectF d => Vector d (a -> b) -> Vector d a -> VectorFamily s d b)

runApply             :: (Arity d, s ~ SelectF d)
                     => proxy d -> Apply d a b s
                     -> Vector d (a -> b) -> Vector d a -> VectorFamily s d b
runApply _ (Apply f) = f


--------------------------------------------------------------------------------

instance (Eq r, Arity d)   => Eq (Vector d r) where
  u == v = F.toList u == F.toList v
-- and $ (==) <$> u <*> v
instance (Ord r, Arity d)  => Ord (Vector d r) where
  u `compare` v = F.toList u `compare` F.toList v

instance (Show r, Arity d) => Show (Vector d r) where
  show v = mconcat [ "Vector", show $ length v , " "
                   , show $ F.toList v
                   ]
-- deriving instance (Arity d, NFData r) => NFData (Vector d r)

-- instance (FromJSON r, Arity d)  => FromJSON (Vector d r) where
--   parseJSON y = parseJSON y >>= \xs -> case vectorFromList xs of
--                   Nothing -> fail . mconcat $
--                     [ "FromJSON (Vector d a), wrong number of elements. Expected "
--                     , show $ natVal (Proxy :: Proxy d)
--                     , " elements but found "
--                     , show $ length xs
--                     , "."
--                     ]
--                   Just v -> pure v

instance (ToJSON r, Arity d) => ToJSON (Vector d r) where
  toJSON     = toJSON     . F.toList
  toEncoding = toEncoding . F.toList


instance Arity d => Additive (Vector d) where
  zero = pure 0
  u ^+^ v = (+) <$> u <*> v

instance Arity d => Affine (Vector d) where
  type Diff (Vector d) = Vector d
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v

instance Arity d => Metric (Vector d)

--------------------------------------------------------------------------------

type instance V.Dim (Vector d)  = d


instance Arity d => V.Vector (Vector d) r where
  -- construct    = Vector <$> runConstruct d (select d c0 c1 c2 c3 c4 cD)
  --   where
  --     d = Proxy :: Proxy d
  --     c0 = undefined
  --     c1 = undefined
  --     c2 = undefined -- Construct $ V.Fun (Vec . L2.V2)
  --     c3 = undefined -- Construct $ V.Fun L3.V3
  --     c4 = undefined -- Construct $ V.Fun L4.V4
  --     cD = Construct $ V.construct
  inspect      = undefined -- V.inspect . _unV
  basicIndex v i = fromMaybe (error e) $ v^?element' i
    where e = "Data.Geometry.VectorFamily.basicIndex: index out of bounds " <> show i

-- cc2 :: Construct d r Two --(d ~ 2) => V.Fun (Peano d) r (VectorFamily Two d r)
-- cc2 = Construct $ V.construct

-- -- type instance V.Dim (Vec d r s)  = d


-- newtype Construct d r s =
--   Construct (V.Fun (Peano (V.Dim (VectorFamily s d))) r (VectorFamily s d r))
-- runConstruct                 :: (d ~ V.Dim (VectorFamily s d))
--                              => proxy d -> Construct d r s
--                              -> V.Fun (Peano d) r (VectorFamily s d r)
-- runConstruct _ (Construct f) = f



-- type instance V.Dim L2.V2 = 2
-- instance V.Vector L2.V2 r where
--   construct = V.Fun L2.V2
--   inspect (L2.V2 x y) (V.Fun f) = f x y
--   basicIndex (L2.V2 x y) = \case
--     0 -> x
--     1 -> y
--     _ -> error "Data.Geometry.Vector.VectorFamily.basicIndex: V2: element out of bounds"

--------------------------------------------------------------------------------


type instance Index   (Vector d r) = Int
type instance IxValue (Vector d r) = r

instance Arity d => Ixed (Vector d r) where
  ix = element'


-- | Lens into the i^th element
element   :: forall proxy i d r. (Arity d, Arity i, (i + 1) <= d)
          => proxy i -> Lens' (Vector d r) r
element i = singular $ element' (fromInteger $ natVal i)

-- | Similar to 'element' above. Except that we don't have a static guarantee
-- that the index is in bounds. Hence, we can only return a Traversal
element'   :: forall d r. Arity d => Int -> Traversal' (Vector d r) r
element' i = unV.l
  where
    d = Proxy :: Proxy d
    l = runElem d $ select d (elem0 i) (elem1 i) (elem2 i) (elem3 i) (elem4 i) (elemD i)


newtype Elem d r s = Elem (Traversal' (VectorFamily s d r) r)
runElem            :: proxy d -> Elem d r s -> Traversal' (VectorFamily s d r) r
runElem _ (Elem t) = t

elem0   :: Int -> Elem d r Zero
elem0 _ = Elem $ \_ v -> pure v
-- zero length vectors don't store any elements

elem1 :: Int -> Elem d r One
elem1 = \case
           0 -> Elem $ lens runIdentity (\_ -> Identity)
           _ -> Elem $ \_ v -> pure v

elem2 :: Int -> Elem d r Two
elem2 = \case
          0 -> Elem L2._x
          1 -> Elem L2._y
          _ -> Elem $ \_ v -> pure v

elem3 :: Int -> Elem d r Three
elem3 = \case
          0 -> Elem L3._x
          1 -> Elem L3._y
          2 -> Elem L3._z
          _ -> Elem $ \_ v -> pure v

elem4 :: Int -> Elem d r Four
elem4 = \case
          0 -> Elem L4._x
          1 -> Elem L4._y
          2 -> Elem L4._z
          3 -> Elem L4._w
          _ -> Elem $ \_ v -> pure v

elemD   :: forall d r. V.Arity d => Int -> Elem d r Many
elemD i = Elem $ FV.element' i





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
cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
(Vector u) `cross` (Vector v) = Vector $ u `L3.cross` v
