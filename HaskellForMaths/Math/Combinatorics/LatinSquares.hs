-- Copyright (c) David Amos, 2009. All rights reserved.

module Math.Combinatorics.LatinSquares where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

-- import Math.Combinatorics.FiniteGeometry
import Math.Combinatorics.Design
import Math.Algebra.Field.Base
import Math.Algebra.Field.Extension
import Math.Algebra.LinearAlgebra (fMatrix')
import Math.Combinatorics.Graph
import Math.Combinatorics.GraphAuts
import Math.Combinatorics.StronglyRegularGraph
import Math.Core.Utils (combinationsOf)


-- LATIN SQUARES

findLatinSqs :: (Eq a) => [a] -> [[[a]]]
findLatinSqs xs = findLatinSqs' 1 [xs] where
    n = length xs
    findLatinSqs' i rows
        | i == n    = [reverse rows]
        | otherwise = concat [findLatinSqs' (i+1) (row:rows)
                             | row <- findRows (L.transpose rows) [] xs]
    findRows (col:cols) ls rs = concat [findRows cols (r:ls) (L.delete r rs)
                                    | r <- rs, r `notElem` col]
    findRows [] ls _ = [reverse ls]

isLatinSq :: (Ord a) => [[a]] -> Bool
isLatinSq rows = all isOneOfEach rows && all isOneOfEach cols where
    cols = L.transpose rows

isOneOfEach xs = length xs == S.size (S.fromList xs)


-- The incidence graph of a latin square
-- It is distance-regular
-- Godsil & Royle p69
incidenceGraphLS l = graph (vs,es) where
    n = length l -- the order of the latin square
    vs = [ (i, j, l ! (i,j)) | i <- [1..n], j <- [1..n] ]
    es = [ [v1,v2] | [v1@(i,j,lij), v2@(i',j',lij')] <- combinationsOf 2 vs, i == i' || j == j' || lij == lij' ]
    m ! (i,j) = m !! (i-1) !! (j-1)

incidenceGraphLS' l = graph (vs,es) where
    n = length l -- the order of the latin square
    vs = [ (i, j) | i <- [1..n], j <- [1..n] ]
    es = [ [v1,v2] | [v1@(i,j), v2@(i',j')] <- combinationsOf 2 vs, i == i' || j == j' || l' M.! (i,j) == l' M.! (i',j') ]
    l' = M.fromList [ ( (i,j), l !! (i-1) !! (j-1) ) | i <- [1..n], j <- [1..n] ]
-- vertices are grid positions
-- adjacent if they're in the same row, same column, or have the same entry


-- ORTHOGONAL AND MUTUALLY ORTHOGONAL LATINS SQUARES

-- |Are the two latin squares orthogonal?
isOrthogonal :: (Ord a, Ord b) => [[a]] -> [[b]] -> Bool
isOrthogonal greeks latins = isOneOfEach pairs
    where pairs = zip (concat greeks) (concat latins)

findMOLS k lsqs = findMOLS' k [] lsqs where
    findMOLS' 0 ls _ = [reverse ls]
    findMOLS' i ls (r:rs) =
        if all (isOrthogonal r) ls
        then findMOLS' (i-1) (r:ls) rs ++ findMOLS' i ls rs
        else findMOLS' i ls rs
    findMOLS' _ _ [] = []

-- |Are the latin squares mutually orthogonal (ie each pair is orthogonal)?
isMOLS :: (Ord a) => [[[a]]] -> Bool
isMOLS (greek:latins) = all (isOrthogonal greek) latins && isMOLS latins
isMOLS [] = True

-- |MOLS from a projective plane
fromProjectivePlane :: (Ord k, Num k) => Design [k] -> [[[Int]]]
fromProjectivePlane (D xs bs) = map toLS parallelClasses where
    k = [x | [0,1,x] <- xs] -- the field we're working over
    n = length k            -- the order of the projective plane
    parallelClasses = drop 2 $ L.groupBy (\l1 l2 -> head l1 == head l2) bs
    -- The classes of parallel lines
    -- Each line has its ideal point at its head
    -- The first two classes have [0,0,1] and [0,1,0] as ideal points, and hence consist of horizontal and vertical lines
    toLS ls = let grid = M.fromList [ ((x,y),i) | (i, [0,1,mu]:ps) <- zip [1..] ls, [1,x,y] <- ps]
              in fMatrix' n (\i j -> grid M.! (k !! i, k !! j))


-- ORTHOGONAL ARRAYS
-- Godsil & Royle p224

isOA (k,n) rows =
    length rows == k &&
    all ( (== n^2) . length ) rows &&
    all isOneOfEach [zip ri rj | [ri,rj] <- combinationsOf 2 rows ]

-- An OA(3,n) from a latin square
fromLS l =
    [ concat [replicate n i | i <- [1..n] ] -- row numbers
    , concat (replicate n [1..n])           -- column numbers
    , concat l                              -- entries
    ]
    where n = length l -- the order of the latin square

fromMOLS mols =
    (concat [replicate n i | i <- [1..n] ]) : -- row numbers
    (concat (replicate n [1..n]) ) :          -- column numbers
    map concat mols                           -- entries for each lsq
    where n = length $ head mols -- the order of the latin squares

-- The graph defined by an OA(k,n)
-- It is strongly regular with parameters ( n^2, (n-1)k, n-2+(k-1)(k-2), k(k-1) )
-- Godsil & Royle p225
graphOA rows = graph (vs,es) where
    vs = L.transpose rows -- the vertices are the columns of the OA
    es = [ [v1,v2] | [v1,v2] <- combinationsOf 2 vs, or (zipWith (==) v1 v2) ]
    -- two vertices are adjacent if they agree in any position

-- Expected SRG parameters
srgParamsOA (k,n) =  Just ( n^2, (n-1)*k, n-2+(k-1)*(k-2), k*(k-1) )

-- eg srgParams (4,4) == srgParams $ graphOA $ init $ fromMOLS $ fromProjectivePlane $ pg2 f4


-- Todo:
-- Would like a way to find out to what extent two sets of MOLS are really the same,
-- eg can one be obtained from the other by row or column reordering (with renumbering)
-- This might provide a proof of the distinctness of phi, omega, omegaD, psi
