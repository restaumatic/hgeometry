{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily where

import           Control.DeepSeq
import           Control.Lens hiding (element)
import           Data.Aeson
import qualified Data.Foldable as F
import           Data.Geometry.Vector (Arity(..))
import qualified Data.Geometry.Vector.VectorFixed as FV
import           Data.Maybe (fromJust)
import           Data.Proxy
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import           Linear.Vector
--------------------------------------------------------------------------------

-- | A proxy which can be used for the coordinates.
data C (n :: Nat) = C deriving (Show,Read,Eq,Ord)

--------------------------------------------------------------------------------
-- * d dimensional Vectors

-- | Datatype representing d dimensional vectors. Our implementation wraps the
-- implementation provided by fixed-vector.
data Vector (d :: Nat) (r :: *) where
  Vector1 :: !r                     -> Vector 1       r
  Vector2 :: !r -> !r               -> Vector 2       r
  Vector3 :: !r -> !r -> !r         -> Vector 3       r
  Vector4 :: !r -> !r -> !r -> !r   -> Vector 4       r
  VectorD :: !(FV.Vector (4 + d) r) -> Vector (4 + d) r

element1   :: forall proxy r. proxy 0 -> Lens' (Vector 1 r) r
element1 _ = lens (\(Vector1 x) -> x) (\_ x -> Vector1 x)

element02   :: forall proxy r. proxy 0 -> Lens' (Vector 2 r) r
element02 _ = lens g s
  where
    g :: Vector 2 r -> r
    g (Vector2 x _) = x
    s :: Vector 2 r -> r -> Vector 2 r
    s (Vector2 _ y) x = Vector2 x y

element12   :: forall proxy r. proxy 1 -> Lens' (Vector 2 r) r
element12 _ = lens g s
  where
    g :: Vector 2 r -> r
    g (Vector2 _ y) = y
    s :: Vector 2 r -> r -> Vector 2 r
    s (Vector2 x _) y = Vector2 x y

-- | Essentially I guess I want a rewrite rule for each of these
element2    :: forall proxy i r. (Arity i, i <= 1)  => proxy i -> Lens' (Vector 2 r) r
element2 px = undefined -- lens get set'
{-# RULES "element/element2" element2 = element02 #-}
{-# RULES "element/element2" element2 = element12 #-}


-- element2   :: forall proxy i r. (Arity i, i <= 2) => proxy i -> Lens' (Vector 2 r) r
-- element2 _ = lens get set''
--   where
--     get (Vector1 x) = x
--     set'' (Vector1 _) x = Vector1 x



-- element0   :: (1 <= d, Arity d) => C 0 -> Lens' (Vector d r) r
-- element0 _ = lens get set''
--   where
--     get (Vector1 x)       = x
--     get (Vector2 x _)     = x
--     get (Vector3 x _ _)   = x
--     get (Vector4 x _ _ _) = x
--     get (VectorD v)       = v^.FV.element (C :: C 0)
--     set'' (Vector1 _)       x = Vector1 x
--     set'' (Vector2 _ y)     x = Vector2 x y
--     set'' (Vector3 _ y z)   x = Vector3 x y z
--     set'' (Vector4 _ y w z) x = Vector4 x y w z
--     set'' (VectorD v)       x = VectorD $ v&FV.element (C :: C 0) .~ x

-- elemT   :: forall i d r. C i -> Lens' (Vector d r) r
-- elemT _ = error "undef"
-- {-# RULES "element/0" elemT = element0 #-}



-- element1   :: (2 <= d, Arity d) => C 1 -> Lens' (Vector d r) r
-- element1 _ = lens get set''
--   where
--     get (Vector2 _ y)     = y
--     get (Vector3 _ y _)   = y
--     get (Vector4 _ y _ _) = y
--     get (VectorD v)       = v^.FV.element (C :: C 1)
--     set'' (Vector2 x _)     y = Vector2 x y
--     set'' (Vector3 x _ z)   y = Vector3 x y z
--     set'' (Vector4 x _ w z) y = Vector4 x y w z
--     set'' (VectorD v)       y = VectorD $ v&FV.element (C :: C 1) .~ y

-- element2   :: (3 <= d, Arity d) => C 2 -> Lens' (Vector d r) r
-- element2 _ = lens get set''
--   where
--     get (Vector3 _ _ z)   = z
--     get (Vector4 _ _ w _) = w
--     get (VectorD v)       = v^.FV.element (C :: C 2)
--     set'' (Vector3 x y _)   z = Vector3 x y z
--     set'' (Vector4 x y _ z) w = Vector4 x y w z
--     set'' (VectorD v)       z = VectorD $ v&FV.element (C :: C 2) .~ z

-- element3   :: (4 <= d, Arity d) => C 3 -> Lens' (Vector d r) r
-- element3 _ = lens get set''
--   where
--     get (Vector4 _ _ _ z) = z
--     get (VectorD v)       = v^.FV.element (C :: C 3)
--     set'' (Vector4 x y w _) z = Vector4 x y w z
--     set'' (VectorD v)       z = VectorD $ v&FV.element (C :: C 3) .~ z

-- elementD    :: forall proxy i d r. (5 <= i, (i+1) <= d, Arity i, Arity d)
--             => proxy i -> Lens' (Vector d r) r
-- elementD px = lens get set''
--   where
--     get (VectorD v)     = v^.FV.element px
--     set'' (VectorD v) z = VectorD $ v&FV.element px .~ z


-- element   :: forall proxy i d r. (Arity i, Arity d, (i+1) <= d)
--           => proxy i -> Lens' (Vector d r) r
-- element _ = error "Data.Geometry.Vector.VectorFamily: element, rewrite rule did not fire."



-- element2'   :: forall proxi i r. (Arity i, i <= 2) -> proxy i -> Lens' (Vector 2 r) r
-- element2' _ = lens get set''
--   where
--     get (Vector2  _) = x
--     set'' (Vector2 _) x = Vector1 x



-- elementL    :: forall proxy i d r. (Arity i, Arity d, (i+1) <= d)
--             => proxy i -> Lens' (Vector d r) r
-- elementL px v = case v of
--   (Vector1 _) -> element1' px
--   (Vector2 _ _) -> element1' px
--   (Vector3 _ _ _) -> element1' px
--   (Vector1 v) -> element1' px







-- vecD               :: Traversal' (Vector d r) (FV.Vector d r)
-- vecD f (VectorD v) = VectorD <$> f v
-- vecD _ v           = pure v

-- destructV :: (c i => proxy i -> Lens' (Vector d r) a) -> Vector d r -> a
-- destructV f = \case
--   (Vector1 x) =


-- element'   :: forall d r. Arity d => Int -> Traversal' (Vector d r) r
-- element' i = case i of
--                  0 -> element0 (C :: C 0)
--                  1 -> element1 (C :: C 1)
--                  2 -> element2 (C :: C 2)
--                  3 -> element3 (C :: C 3)
--                  _ -> vecD.FV.element' i


-- -- | similar to 'element' above. Except that we don't have a static guarantee
-- -- that the index is in bounds. Hence, we can only return a Traversal
-- element'   :: forall d r. Arity d => Int -> Traversal' (Vector d r) r
-- element' i f v
--   | 0 <= i && i < bnd v) = f (get i)
--                                                  <&> \a -> (v&lns .~ a)
--        -- Implementation based on that of Ixed Vector in Control.Lens.At
--   | otherwise                                     = pure v
--   where
--     (get,set,b) = case natVal (C :: C d) of
--                     2 -> undefined
--                     3 -> undefined




-- vectorFromList :: Arity d => [a] -> Maybe (Vector d a)
-- vectorFromList = \case
--   [x]       -> f 1 $ Vector1 x
--   [x,y]     -> f 2 $ Vector2 x y
--   [x,y,z]   -> f 3 $ Vector3 x y z
--   [x,y,w,z] -> f 4 $ Vector4 x y w z
--   xs        -> VectorD <$> FV.vectorFromList xs
--   where
--     d = natVal (C :: C d)
--     f i x = if i == d then Just x else Nothing


-- vectorFromListUnsafe :: Arity d => [a] -> Vector d a
-- vectorFromListUnsafe = fromJust . vectorFromList


-- instance (Show r, Arity d) => Show (Vector d r) where
--   show (Vector v) = mconcat [ "Vector", show $ V.length v , " "
--                             , show $ F.toList v
--                             ]

-- deriving instance (Eq r, Arity d)   => Eq (Vector d r)
-- deriving instance (Ord r, Arity d)  => Ord (Vector d r)
-- -- deriving instance Arity d  => Functor (Vector d)

-- -- for some weird reason, implemeting this myself yields is faster code
-- instance Arity d  => Functor (Vector d) where
--   fmap f (Vector v) = Vector $ fmap f v

-- deriving instance Arity d  => Foldable (Vector d)
-- deriving instance Arity d  => Applicative (Vector d)

-- instance Arity d => Traversable (Vector d) where
--   traverse f (Vector v) = Vector <$> traverse f v

-- deriving instance (Arity d, NFData r) => NFData (Vector d r)


-- instance Arity d => Additive (Vector d) where
--   zero = pure 0
--   (Vector u) ^+^ (Vector v) = Vector $ V.zipWith (+) u v

-- instance Arity d => Affine (Vector d) where
--   type Diff (Vector d) = Vector d

--   u .-. v = u ^-^ v
--   p .+^ v = p ^+^ v


-- instance Arity d => Metric (Vector d)

-- type instance V.Dim (Vector d) = d

-- instance Arity d => V.Vector (Vector d) r where
--   construct  = Vector <$> V.construct
--   inspect    = V.inspect . _unV
--   basicIndex = V.basicIndex . _unV

-- instance (FromJSON r, Arity d, KnownNat d)  => FromJSON (Vector d r) where
--   parseJSON y = parseJSON y >>= \xs -> case vectorFromList xs of
--                   Nothing -> fail . mconcat $
--                     [ "FromJSON (Vector d a), wrong number of elements. Expected "
--                     , show $ natVal (Proxy :: Proxy d)
--                     , " elements but found "
--                     , show $ length xs
--                     , "."
--                     ]
--                   Just v -> pure v

-- instance (ToJSON r, Arity d) => ToJSON (Vector d r) where
--   toJSON     = toJSON     . F.toList
--   toEncoding = toEncoding . F.toList

-- ------------------------------------------

-- -- | Get the head and tail of a vector
-- destruct            :: (Arity d, Arity (d + 1), 1 <= (d + 1))
--                     => Vector (d + 1) r -> (r, Vector d r)
-- destruct (Vector v) = (V.head v, Vector $ V.tail v)


-- -- | Cross product of two three-dimensional vectors
-- cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
-- u `cross` v = fromV3 $ (toV3 u) `L3.cross` (toV3 v)


-- --------------------------------------------------------------------------------

-- -- | Vonversion to a Linear.V2
-- toV2                :: Vector 2 a -> L2.V2 a
-- toV2 ~(Vector2 a b) = L2.V2 a b

-- -- | Conversion to a Linear.V3
-- toV3                  :: Vector 3 a -> L3.V3 a
-- toV3 ~(Vector3 a b c) = L3.V3 a b c

-- -- | Conversion from a Linear.V3
-- fromV3               :: L3.V3 a -> Vector 3 a
-- fromV3 (L3.V3 a b c) = v3 a b c

-- ----------------------------------------------------------------------------------

-- -- | Add an element at the back of the vector
-- snoc :: (Arity (d + 1), Arity d) => Vector d r -> r -> Vector (d + 1) r
-- snoc = flip V.snoc

-- -- | Get a vector of the first d - 1 elements.
-- init :: (Arity d, Arity (d + 1)) => Vector (d + 1) r -> Vector d r
-- init = Vector . V.reverse . V.tail . V.reverse . _unV

-- last :: forall d r. (Arity d, Arity (d + 1)) => Vector (d + 1) r -> r
-- last = view $ element (Proxy :: Proxy d)

-- type Prefix i d = ( Peano (i + 1) ~ S (Peano i)
--                   , Peano (d + 1) ~ S (Peano d)
--                   , KnownNat i, Arity i)

-- -- | Get a prefix of i elements of a vector
-- prefix :: forall i d r. (Arity d, Prefix i d, i <= d)
--        => Vector d r -> Vector i r
-- prefix = let i = fromInteger . natVal $ (Proxy :: Proxy i)
--          in V.fromList . take i . V.toList

--------------------------------------------------------------------------------
-- * Functions specific to two and three dimensional vectors.

-- -- | Construct a 2 dimensional vector
-- v2 :: r -> r -> Vector 2 r
-- v2 = Vector2

-- -- | Construct a 3 dimensional vector
-- v3 :: r -> r -> r -> Vector 3 r
-- v3 = Vector3
