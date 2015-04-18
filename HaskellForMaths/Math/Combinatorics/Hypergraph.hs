-- Copyright (c) 2009, David Amos. All rights reserved.

-- |A module defining a type for hypergraphs.
module Math.Combinatorics.Hypergraph where

import qualified Data.List as L
import Math.Common.ListSet
import Math.Core.Utils (combinationsOf)
import Math.Combinatorics.Graph hiding (incidenceMatrix)
import Math.Algebra.Group.PermutationGroup (orbitB, p) -- needed for construction of Coxeter group

-- not used in this module, only in GHCi
import Math.Algebra.Field.Base
import Math.Algebra.Field.Extension
import Math.Combinatorics.Design hiding (incidenceMatrix, incidenceGraph, dual, isSubset, fanoPlane)


-- set system or hypergraph
data Hypergraph a = H [a] [[a]] deriving (Eq,Ord,Show)

hypergraph xs bs | isSetSystem xs bs = H xs bs

toHypergraph xs bs = H xs' bs' where
    xs' = L.sort xs
    bs' = L.sort $ map L.sort bs
-- this still doesn't guarantee that all bs are subset of xs


-- |Is this hypergraph uniform - meaning that all blocks are of the same size
isUniform :: (Ord a) => Hypergraph a -> Bool
isUniform h@(H xs bs) = isSetSystem xs bs && same (map length bs)

same (x:xs) = all (==x) xs
same [] = True



fromGraph (G vs es) = H vs es
fromDesign (D xs bs) = H xs (L.sort bs)
-- !! should insist that designs have blocks in order
-- !! dual probably doesn't guarantee this at present

{-
dual (H xs bs) = toHypergraph (bs, map beta xs) where
    beta x = filter (x `elem`) bs
-}



-- INCIDENCE GRAPH

-- data Incidence a = P a | B [a] deriving (Eq, Ord, Show)

-- compare Design, where we just use Left, Right

-- Also called the Levi graph
incidenceGraph :: (Ord a) => Hypergraph a -> Graph (Either a [a])
incidenceGraph (H xs bs) = G vs es where
    vs = map Left xs ++ map Right bs
    es = L.sort [ [Left x, Right b] | b <- bs, x <- b]


-- INCIDENCE MATRIX

-- !! why are we doing this the other way round to the literature ??


-- incidence matrix of a hypergraph
-- (rows and columns indexed by edges and vertices respectively)
-- (warning: in the literature it is often the other way round)
incidenceMatrix (H vs es) = [ [if v `elem` e then 1 else 0 | v <- vs] | e <- es]

fromIncidenceMatrix m = H vs es where
    n = L.genericLength $ head m
    vs = [1..n]
    es = L.sort $ map edge m
    edge row = [v | (1,v) <- zip row vs]




-- isTwoGraph


-- We can represent various incidence structures as hypergraphs,
-- by identifying the lines with the sets of points that they contain

isPartialLinearSpace :: (Ord a) => Hypergraph a -> Bool
isPartialLinearSpace h@(H ps ls) =
    isSetSystem ps ls &&
    all ( (<=1) . length ) [filter (pair `isSubset`) ls | pair <- combinationsOf 2 ps]
    -- any two points are incident with at most one line

-- Godsil & Royle, p79
-- |Is this hypergraph a projective plane - meaning that any two lines meet in a unique point,
-- and any two points lie on a unique line
isProjectivePlane :: (Ord a) => Hypergraph a -> Bool
isProjectivePlane h@(H ps ls) =
    isSetSystem ps ls &&
    all ( (==1) . length) [intersect l1 l2 | [l1,l2] <- combinationsOf 2 ls] && -- any two lines meet in a unique point
    all ( (==1) . length) [ filter ([p1,p2] `isSubset`) ls | [p1,p2] <- combinationsOf 2 ps] -- any two points lie in a unique line

-- |Is this hypergraph a projective plane with a triangle.
-- This is a weak non-degeneracy condition, which eliminates all points on the same line, or all lines through the same point.
isProjectivePlaneTri :: (Ord a) => Hypergraph a -> Bool
isProjectivePlaneTri h@(H ps ls) =
    isProjectivePlane h && any triangle (combinationsOf 3 ps)
    where triangle t@[p1,p2,p3] =
                   (not . null) [l | l <- ls, [p1,p2] `isSubset` l, p3 `notElem` l] && -- there is a line containing p1,p2 but not p3
                   (not . null) [l | l <- ls, [p1,p3] `isSubset` l, p2 `notElem` l] &&
                   (not . null) [l | l <- ls, [p2,p3] `isSubset` l, p1 `notElem` l] 

-- |Is this hypergraph a projective plane with a quadrangle.
-- This is a stronger non-degeneracy condition.
isProjectivePlaneQuad :: (Ord a) => Hypergraph a -> Bool
isProjectivePlaneQuad h@(H ps ls) =
    isProjectivePlane h && any quadrangle (combinationsOf 4 ps)
    where quadrangle q = all (not . collinear) (combinationsOf 3 q) -- no three points collinear
          collinear ps = any (ps `isSubset`) ls


-- > isProjectivePlaneQuad $ fromDesign $ pg2 f2
-- True


-- GENERALIZED QUADRANGLES

-- Godsil & Royle p81
isGeneralizedQuadrangle :: (Ord a) => Hypergraph a -> Bool
isGeneralizedQuadrangle h@(H ps ls) =
    isPartialLinearSpace h &&
    all (\(l,p) -> unique [p' | p' <- l, collinear (pair p p')]) [(l,p) | l <- ls, p <- ps, p `notElem` l] &&
    -- given any line l and point p not on l, there is a unique point p' on l with p and p' collinear
    any (not . collinear) (powerset ps) && -- there are non collinear points
    any (not . concurrent) (powerset ls) -- there are non concurrent lines
    where unique xs = length xs == 1
          pair x y = if x < y then [x,y] else [y,x]
          collinear ps = any (ps `isSubset`) ls
          concurrent ls = any (\p -> all (p `elem`) ls) ps


grid m n = H ps ls where
    ps = [(i,j) | i <- [1..m], j <- [1..n] ]
    ls = L.sort $ [ [(i,j) | i <- [1..m] ] | j <- [1..n] ] -- horizontal lines
               ++ [ [(i,j) | j <- [1..n] ] | i <- [1..m] ] -- vertical lines

dualGrid m n = fromGraph $ kb m n
-- the lines of the grid are the points of the dual, and the points of the grid are the lines of the dual

isGenQuadrangle' h = diameter g == 4 && girth g == 8 -- !! plus non-degeneracy conditions
    where g = incidenceGraph h


-- CONFIGURATIONS

-- http://en.wikipedia.org/wiki/Projective_configuration
-- |Is this hypergraph a (projective) configuration.
isConfiguration :: (Ord a) => Hypergraph a -> Bool
isConfiguration h@(H ps ls) =
    isUniform h && -- a set system, with each line incident with the same number of points
    same [length (filter (p `elem`) ls) | p <- ps] -- each point is incident with the same number of lines


fanoPlane :: Hypergraph Integer
fanoPlane = toHypergraph [1..7] [[1,2,4],[2,3,5],[3,4,6],[4,5,7],[5,6,1],[6,7,2],[7,1,3]]

-- |The Heawood graph is the incidence graph of the Fano plane
heawoodGraph :: Graph (Either Integer [Integer])
heawoodGraph = incidenceGraph fanoPlane


desarguesConfiguration :: Hypergraph [Integer]
desarguesConfiguration = H xs bs where
    xs = combinationsOf 2 [1..5]
    bs = [ [x | x <- xs, x `isSubset` b] | b <- combinationsOf 3 [1..5] ]

desarguesGraph :: Graph (Either [Integer] [[Integer]])
desarguesGraph = incidenceGraph desarguesConfiguration


pappusConfiguration :: Hypergraph Integer
pappusConfiguration = H xs bs where
    xs = [1..9]
    bs = L.sort [ [1,2,3], [4,5,6], [7,8,9], [1,5,9], [1,6,8], [2,4,9], [3,4,8], [2,6,7], [3,5,7] ]

pappusGraph :: Graph (Either Integer [Integer])
pappusGraph = incidenceGraph pappusConfiguration



-- !! no particular reason why the following is here rather than elsewhere
{-
triples = combinationsOf 3 [1..7]

heptads = [ [a,b,c,d,e,f,g] | a <- triples,
                              b <- triples, a < b, meetOne b a,
                              c <- triples, b < c, all (meetOne c) [a,b],
                              d <- triples, c < d, all (meetOne d) [a,b,c],
                              e <- triples, d < e, all (meetOne e) [a,b,c,d],
                              f <- triples, e < f, all (meetOne f) [a,b,c,d,e],
                              g <- triples, f < g, all (meetOne g) [a,b,c,d,e,f],
                              foldl intersect [1..7] [a,b,c,d,e,f,g] == [] ]
    where meetOne x y = length (intersect x y) == 1
    -- each pair of triples meet in exactly one point, and there is no point in all of them - Godsil & Royle p69
    -- (so these are the projective planes over 7 points)
-}
-- Godsil & Royle p69
coxeterGraph :: Graph [Integer]
coxeterGraph = G vs es where
    g = p [[1..7]]
    vs = L.sort $ concatMap (orbitB [g]) [[1,2,4],[3,5,7],[3,6,7],[5,6,7]]
    es = [ e | e@[v1,v2] <- combinationsOf 2 vs, disjoint v1 v2]

-- is this the incidence graph of a hypergraph involving heptads over triples?


-- edges of K6
duads = combinationsOf 2 [1..6]

-- 1-factors of K6
-- 15 different ways to pick three disjoint duads from [1..6]
synthemes = [ [d1,d2,d3] | d1 <- duads,
                           d2 <- duads, d2 > d1, disjoint d1 d2,
                           d3 <- duads, d3 > d2, disjoint d1 d3, disjoint d2 d3 ]

-- |The Tutte-Coxeter graph, also called the Tutte 8-cage
tutteCoxeterGraph :: Graph (Either [Integer] [[Integer]])
tutteCoxeterGraph = incidenceGraph $ H duads synthemes


-- Also known as line graph
intersectionGraph (H xs bs) = G vs es where
    vs = bs
    es = [pair | pair@[b1,b2] <- combinationsOf 2 bs, not (disjoint b1 b2)]