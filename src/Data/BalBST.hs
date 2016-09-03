{-# LANGUAGE RecordWildCards #-}
module Data.BalBST where

import Data.Bifunctor
import Data.Function(on)
import Prelude hiding (lookup,null)
import Data.Maybe(isJust,fromJust,mapMaybe)

import qualified Data.Tree as T

data TreeNavigator k a = Nav { goLeft     :: a -> k -> Bool
                             , extractKey :: a -> a -> k
                             }

ordNav :: Ord a => TreeNavigator a a
ordNav = Nav (<=) min


ordNavBy   :: Ord b => (a -> b) ->  TreeNavigator b a
ordNavBy f = Nav (\x k -> f x <= k) (min `on` f)


-- instance Functor (TreeNavigator k) where
--   fmap f Nav{..} = Nav (\b k -> )




data BalBST k a = BalBST { nav    :: !(TreeNavigator k a)
                         , toTree :: !(Tree k a)
                         }


instance (Show k, Show a) => Show (BalBST k a) where
  show (BalBST _ t) = "BalBST (" ++ show t ++ ")"

-- instance Functor (BalBST k) where





data Color = Red | Black deriving (Show,Read,Eq,Ord)

type Height = Int

-- Red-Black tree with values in the leaves
data Tree k a = Empty
              | Leaf !a
              | Node !Color !Height (Tree k a) !k (Tree k a) deriving (Show,Eq)


--------------------------------------------------------------------------------

-- | Check if the tree is empty
null                  :: BalBST k a -> Bool
null (BalBST _ Empty) = True
null _                = False

lookup :: Eq a => a -> BalBST k a -> Maybe a
lookup x (BalBST Nav{..} t) = lookup' t
  where
    lookup' Empty            = Nothing
    lookup' (Leaf y)         = if x == y then Just y else Nothing
    lookup' (Node _ _ l k r)
      | goLeft x k           = lookup' l
      | otherwise            = lookup' r







member   :: Eq a => a -> BalBST k a -> Bool
member x = isJust . lookup x






insert :: a -> BalBST k a -> BalBST k a
insert x (BalBST n@Nav{..} t) = BalBST n (blacken $ insert' t)
  where
    insert' Empty    = Leaf x
    insert' (Leaf y) = let k     = extractKey x y
                           (l,r) = if goLeft x k then (x,y) else (y,x)
                       in red 2 (Leaf l) k (Leaf r)
    insert' (Node c h l k r)
      | goLeft  x k  = balance c h (insert' l) k r
      | otherwise    = balance c h l           k (insert' r)


balance  :: Color -> Height -> Tree k a -> k -> Tree k a -> Tree k a
balance Black h (Node Red _ (Node Red _ a x b) y c) z d = mkNode h a x b y c z d
balance Black h (Node Red _ a x (Node Red _ b y c)) z d = mkNode h a x b y c z d
balance Black h a x (Node Red _ (Node Red _ b y c) z d) = mkNode h a x b y c z d
balance Black h a x (Node Red _ b y (Node Red _ c z d)) = mkNode h a x b y c z d
balance co h a x b                                      = Node co h a x b

mkNode                 :: Height
                       -> Tree k a -> k -> Tree k a -> k -> Tree k a  -> k -> Tree k a
                       -> Tree k a
mkNode h a x b y c z d = red h (black h a x b) y (black h c z d)



-- delete                        :: Eq a => a -> BalBST k a -> BalBST k a
-- delete x (BalBST n@Nav{..} t) = delete' t
--   where
--     delete' Empty      = Empty
--     delete' l@(Leaf y) = if x == y then Empty else l
--     delete' (Node c h l k r)
--       | goLeft x k     =


--------------------------------------------------------------------------------

black :: Height -> Tree k a -> k -> Tree k a -> Tree k a
black = Node Black

red :: Height -> Tree k a -> k -> Tree k a -> Tree k a
red = Node Red


blacken                    :: Tree k a -> Tree k a
blacken (Node Red h l k r) = Node Black h l k r
blacken t                  = t

height                  :: Tree k a -> Height
height Empty            = 0
height (Leaf _)         = 1
height (Node _ h _ _ _) = h


minView              :: BalBST k a -> Maybe (a, Tree k a)
minView (BalBST n t) = minView' t
  where
    minView' Empty            = Nothing
    minView' (Leaf x)         = Just (x,Empty)
    minView' (Node _ _ l _ r) = fmap (flip (joinWith n) r) <$> minView' l

maxView              :: BalBST k a -> Maybe (a, Tree k a)
maxView (BalBST n t) = maxView' t
  where
    maxView' Empty            = Nothing
    maxView' (Leaf x)         = Just (x,Empty)
    maxView' (Node _ _ l _ r) = fmap (joinWith n l) <$> maxView' r

-- | takes the left Tree nav
join                           :: BalBST k a -> BalBST k a -> BalBST k a
join (BalBST n l) (BalBST _ r) = BalBST n $ joinWith n l r

joinWith               :: TreeNavigator k a -> Tree k a -> Tree k a -> Tree k a
joinWith Nav{..} tl tr
    | lh >= rh         = blacken $ joinL tl tr
    | otherwise        = blacken $ joinR tl tr
  where
    rh = height tr
    lh = height tl

    joinL Empty      _           = Empty
    joinL l          Empty       = l
    joinL l@(Leaf x) r@(Leaf y)  = red 2 l (extractKey x y) r
    joinL l@(Node c h ll k lr) r
      | h == rh                  = let lm = unsafeMax lr
                                       rm = unsafeMin r
                                   in balance Red (h+1) l (extractKey lm rm) r
      | otherwise                = balance c h ll k (joinL lr r)
        -- lh >= rh
    joinL _ _ = error "joinL. absurd"


    joinR _          Empty       = Empty
    joinR Empty      r           = r

    joinR l@(Leaf x) r@(Leaf y)  = red 2 l (extractKey x y) r
    joinR l r@(Node c h rl k rr)
      | h == lh                  = let lm = unsafeMax l
                                       rm = unsafeMin rl
                                   in balance Red (h+1) l (extractKey lm rm) r
      | otherwise                = balance c h (joinR l rl) k rr
        -- lh >= rh
    joinR _ _ = error "joinR absurd"



data Pair a b = Pair !a b deriving (Show,Eq,Functor,Foldable,Traversable)


extractPrefix                      :: BalBST k a -> [Pair a (Tree k a)]
extractPrefix (BalBST n@Nav{..} t) = extractPrefix' t
  where
    extractPrefix' Empty            = []
    extractPrefix' (Leaf x)         = [Pair x Empty]
    extractPrefix' (Node _ _ l _ r) = ls ++ extractPrefix' r
      where
        ls = map (fmap $ flip (joinWith n) r) $ extractPrefix' l

extractSuffix                      :: BalBST k a -> [Pair a (Tree k a)]
extractSuffix (BalBST n@Nav{..} t) = extract t
  where
    extract Empty            = []
    extract (Leaf x)         = [Pair x Empty]
    extract (Node _ _ l _ r) = rs ++ extract l
      where
        rs = map (fmap $ joinWith n l) $ extract r


data Split a b = Split a !b a

-- | Splits the tree at x. Note that if x occurs more often, no guarantees are
-- given which one is found.
split                        :: Eq a => a -> BalBST k a -> Split (Tree k a) (Maybe a)
split x (BalBST n@Nav{..} t) = split' t
  where
    split' Empty                  = Split Empty Nothing Empty
    split' l@(Leaf y)
      | x == y                    = Split Empty (Just y) Empty
      | goLeft x (extractKey x y) = Split l     Nothing  Empty
      | otherwise                 = Split Empty Nothing  l
    split' (Node _ _ l k r)
      | goLeft x k                = let Split l' mx r' = split' l
                                    in Split l' mx (joinWith n r' r)
      | otherwise                 = let Split l' mx r' = split' r
                                    in Split (joinWith n l l') mx r'

-- | split based on a monotonic predicate
splitMonotone                        :: (a -> Bool) -> BalBST k a
                                     -> (BalBST k a, BalBST k a)
splitMonotone p (BalBST n@Nav{..} t) = bimap (BalBST n) (BalBST n) $ split' t
  where
    split' Empty        = (Empty,Empty)
    split' l@(Leaf y)
      | p y             = (Empty,l)
      | otherwise       = (l,Empty)
    split' (Node _ _ l _ r)
      | p (unsafeMin r) = let (l',m) = split' l in (l',joinWith n m r)
      | otherwise       = let (m,r') = split' r in (joinWith n l m, r')



empty   :: TreeNavigator k a -> BalBST k a
empty n = BalBST n Empty

--------------------------------------------------------------------------------

-- | $O(n\log n)$
fromList :: TreeNavigator k a -> [a] -> BalBST k a
fromList n = foldr insert (empty n)

fromList' :: Ord a => [a] -> BalBST a a
fromList' = fromList ordNav


-- | $O(n)$
fromAscList :: TreeNavigator k a -> [a] -> BalBST k a
fromAscList = undefined


data T k a = Internal !Color !Height !k | Val !a deriving (Show,Eq,Ord)

toRoseTree :: Tree k a -> Maybe (T.Tree (T k a))
toRoseTree Empty            = Nothing
toRoseTree (Leaf x)         = Just $ T.Node (Val x) []
toRoseTree (Node c h l k r) = Just $ T.Node (Internal c h k) (mapMaybe toRoseTree [l,r])


showTree :: (Show k, Show a) => BalBST k a -> String
showTree = maybe "Empty" T.drawTree . fmap (fmap show) . toRoseTree . toTree


unsafeMin                  :: Tree k a -> a
unsafeMin (Leaf x)         = x
unsafeMin (Node _ _ l _ _) = unsafeMin l
unsafeMin _                = error "unsafeMin: Empty"

unsafeMax                  :: Tree k a -> a
unsafeMax (Leaf x)         = x
unsafeMax (Node _ _ _ _ r) = unsafeMax r
unsafeMax _                = error "unsafeMax: Empty"

toList = toList' . toTree

toList'                  :: Tree k a -> [a]
toList' Empty            = []
toList' (Leaf x)         = [x]
toList' (Node _ _ l _ r) = toList' l ++ toList' r