{-
 - mod11pa.hs -- CS 430 Spring 2018 Module 11 PA.
 - author: Mitchell Mesecher
 -}

module Mod11PA where

{-------------------------------------------------------------------------------
 - Part 1: Generate CCW triples of points from a list of points.
 ------------------------------------------------------------------------------}

-- The type for a single point.
type Point a = (a,a)

-- The type for a pair of points.
type Pair a = (Point a, Point a)

-- The type for a triple of points.
type Triple a = (Point a, Point a, Point a)

-- Convert a triple to a CCW triple
toCCW :: Real a => Triple a -> Triple a
toCCW (a,b,c)
    | isCCW t = t --check if already isCCW
    | otherwise = (a,c,b)
    where
       t = (a,b,c)
       
-- Generate all pairs of points from a list of points.
-- Each pair should appear exactly once in the result.
pairsFromPoints :: Real a => [Point a] -> [Pair a]
pairsFromPoints (n:ns) = map pairs ns ++ pairsFromPoints ns --map all point combinations
    where pairs m = (n,m) 
pairsFromPoints _ = []
        
-- Generate all unique CCW triples of points from a list of points
-- Each triple should appear exactly once in the result and be
-- CCW ordered.
triplesFromPoints :: Real a => [Point a] -> [Triple a]
triplesFromPoints (n:ns) = map pairToCCWTriple (map fn (pairsFromPoints ns)) ++ triplesFromPoints ns
    where fn = ((,) n) --append point to pair object
triplesFromPoints _ = []
    
--Helper function to convert point pair object to CCW triple
pairToCCWTriple :: Real a => (Point a, Pair a) -> Triple a 
pairToCCWTriple (a,(b,c)) = toCCW (a,b,c)  
         
{-------------------------------------------------------------------------------
 - Part 2: Find the Delaunay triangulation of a set of points.
 ------------------------------------------------------------------------------}

-- Determines whether a triple is a delaunay triangle or not
isDelaunayTriangle :: Real a => [Point a] -> Triple a -> Bool
isDelaunayTriangle [] a = True
isDelaunayTriangle (n:ns) a --recursively check all points against triangle
    | isInCircle n a = False 
    | otherwise = isDelaunayTriangle ns a 


-- Returns the triangles of the delaunay triangulation of a list of points
-- as a list of ccw-oriented triples of points. Each triple of points is the
-- endpoints of one triangle in the triangulation.
delaunayTriangulation :: Real a => [Point a] -> [Triple a]
delaunayTriangulation n = filter (isDelaunayTriangle n) (triplesFromPoints(n)) --filter triples that are Delaunay Triangles 


{-------------------------------------------------------------------------------
 - Provided helper functions; DO NOT MODIFY BELOW THIS LINE
 ------------------------------------------------------------------------------}

-- Determines whether a point is in the circle defined by a triple of points
isInCircle :: Real a => Point a -> Triple a -> Bool
isInCircle (px, py) ((p1x, p1y), (p2x, p2y), (p3x, p3y)) =
  p /= p1 && p /= p2 && p /= p3 &&
  ((det4 p1x p1y ((p1x*p1x)+(p1y*p1y)) 1
        p2x p2y ((p2x*p2x)+(p2y*p2y)) 1
        p3x p3y ((p3x*p3x)+(p3y*p3y)) 1
        px py ((px*px)+(py*py)) 1) > 0)
  where
    p = (px, py)
    p1 = (p1x, p1y)
    p2 = (p2x, p2y)
    p3 = (p3x, p3y)

-- Predicate for a triple of 3 points is in CCW order or not
isCCW :: Real a => Triple a -> Bool
isCCW ((x1, y1), (x2, y2), (x3, y3)) = (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1) > 0

-- 2x2 determinants
det2 :: Real a => a -> a -> a -> a -> a
det2 a b c d = a*d - b*c

-- 3x3 determinants
det3 :: Real a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
det3 a b c d e f g h i = a * (det2 e f h i) - b * (det2 d f g i) + c * (det2 d e g h)

-- 4x4 determinants
det4 :: Real a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
det4 a b c d e f g h i j k l m n o p =
    a * (det3 f g h j k l n o p) -
    b * (det3 e g h i k l m o p) +
    c * (det3 e f h i j l m n p) -
    d * (det3 e f g i j k m n o)

