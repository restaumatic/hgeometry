module Data.Geometry.Polygon.Convex( ConvexPolygon
                                   , merge
                                   , lowerTangent, upperTangent
                                   , isLeftOf, isRightOf
                                   ) where

import Control.Lens
import Data.Ext
import Data.Function(on, )
import Data.Geometry
import Data.Geometry.Polygon(fromPoints)
import qualified Data.CircularSeq as C
import Data.CircularSeq(focus,CSeq)
import qualified Data.Foldable as F
import Data.Maybe(fromJust)
import Data.Ord(comparing)

--------------------------------------------------------------------------------

type ConvexPolygon = SimplePolygon




-- * Merging Two convex Hulls


-- | Rotating Right <-> rotate clockwise
--
-- Merging two convex hulls, based on the paper:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980
--
-- : (combined hull, lower tangent that was added, upper tangent thtat was
-- added)

-- pre: - lp and rp are disjoint, and there is a vertical line separating
--        the two polygons.
--      - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
merge       :: (Num r, Ord r) => ConvexPolygon p r  -> ConvexPolygon p r
            -> (ConvexPolygon p r, LineSegment 2 p r, LineSegment 2 p r)
merge lp rp = (fromPoints $ r' ++ l', lt, ut)
  where
    lt@(ClosedLineSegment a b) = lowerTangent lp rp
    ut@(ClosedLineSegment c d) = upperTangent lp rp


    takeUntil p xs = let (xs',x:_) = break p xs in xs' ++ [x]
    rightElems  = F.toList . C.rightElements

    r' = takeUntil (coreEq b) . rightElems . rotateTo' d $ rp^.outerBoundary
    l' = takeUntil (coreEq c) . rightElems . rotateTo' a $ lp^.outerBoundary


rotateTo'   :: Eq a => (a :+ b) -> CSeq (a :+ b) -> CSeq (a :+ b)
rotateTo' x = fromJust . C.findRotateTo (coreEq x)


coreEq :: Eq a => (a :+ b) -> (a :+ b) -> Bool
coreEq = (==) `on` (^.core)

-- | Compute the lower tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
lowerTangent                                     :: (Num r, Ord r)
                                                 => ConvexPolygon p r
                                                 -> ConvexPolygon p r
                                                 -> LineSegment 2 p r
lowerTangent (SimplePolygon l) (SimplePolygon r) = rotate xx yy zz zz''
  where
    xx = rightMost l
    yy = leftMost r

    zz   = pred' yy
    zz'' = succ' xx

    rotate x y z z''
      | focus z   `isRightOf` (focus x, focus y) = rotate x   z (pred' z) z''
                                                      -- rotate the right polygon CCW
      | focus z'' `isRightOf` (focus x, focus y) = rotate z'' y z         (succ' z'')
                                                      -- rotate the left polygon CW
      | otherwise                                = ClosedLineSegment (focus x)
                                                                     (focus y)

succ' :: CSeq a -> CSeq a
succ' = C.rotateR

pred' :: CSeq a -> CSeq a
pred' = C.rotateL

-- | Compute the upper tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
upperTangent                                     :: (Num r, Ord r)
                                                 => ConvexPolygon p r
                                                 -> ConvexPolygon p r
                                                 -> LineSegment 2 p r
upperTangent (SimplePolygon l) (SimplePolygon r) = rotate xx yy zz zz'
  where
    xx = rightMost l
    yy = leftMost r

    zz  = succ' yy
    zz' = pred' xx

    rotate x y z z'
      | focus z  `isLeftOf` (focus x, focus y) = rotate x  z (succ' z) z'
                                                    -- rotate the right polygon CW
      | focus z' `isLeftOf` (focus x, focus y) = rotate z' y z        (pred' z')
                                                    -- rotate the left polygon CCW
      | otherwise                              = ClosedLineSegment (focus x)
                                                                   (focus y)

isRightOf           :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isRightOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CW

isLeftOf            :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isLeftOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CCW


--------------------------------------------------------------------------------

-- | Rotate to the rightmost point
rightMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
rightMost xs = let m = F.maximumBy (comparing (^.core.xCoord)) xs in rotateTo' m xs

-- | Rotate to the leftmost point
leftMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
leftMost xs = let m = F.minimumBy (comparing (^.core.xCoord)) xs in rotateTo' m xs


-- -- | rotate right while p 'current' 'rightNeibhour' is true
-- rotateRWhile      :: (a -> a -> Bool) -> C.CList a -> C.CList a
-- rotateRWhile p lst
--   | C.isEmpty lst = lst
--   | otherwise     = go lst
--     where
--       go xs = let cur = focus xs
--                   xs' = C.rotR xs
--                   nxt = focus' xs'
--               in if p cur nxt then go xs' else xs
