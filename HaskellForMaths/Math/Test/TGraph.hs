-- Copyright (c) David Amos, 2008. All rights reserved.

{-# LANGUAGE FlexibleInstances #-}

module Math.Test.TGraph where

import qualified Data.List as L

import Math.Combinatorics.Graph as G
import Math.Combinatorics.StronglyRegularGraph as SRG
import Math.Combinatorics.Hypergraph as H
import Math.Combinatorics.GraphAuts
import Math.Algebra.Group.PermutationGroup as P -- not used
import Math.Algebra.Group.SchreierSims as SS

-- import Math.Algebra.Group.StringRewriting
import Math.Algebra.Group.CayleyGraph


-- Sources
-- [AGT] - Godsil and Royle, Algebraic Graph Theory

factorial n = product [1..n]

choose n m | m <= n = product [m+1..n] `div` product [1..n-m]

test = and [graphPropsTest, graphTransitivityTest, srgParamTest, graphAutTest]


graphPropsTest = all (uncurry (==)) graphPropsTestsBool
              && all (uncurry (==)) graphPropsTestsInt

graphPropsTestsBool =
    -- [(isConnected nullGraph, True)] ++
    [(isConnected (c n), True) | n <- [3..8] ] ++
    [(isConnected $ complement $ k n, False) | n <- [3..6] ]

graphPropsTestsInt =
    [(diameter (c n), n `div` 2) | n <- [3..8] ] ++
    [(girth (c n), n) | n <- [3..8] ] ++
    [(girth (kb m n), 4) | m <- [2..4], n <- [2..4] ] ++
    [(girth petersen, 5), (girth heawoodGraph, 6), (girth coxeterGraph, 7), (girth tutteCoxeterGraph, 8)]

graphTransitivityTest = and graphTransitivityTests

graphTransitivityTests =
    [(not . isVertexTransitive) (kb m n) | n <- [1..3], m <- [1..3], m /= n] ++
    [isEdgeTransitive (kb m n) | n <- [1..3], m <- [1..3]] ++
    map isArcTransitive [k 4, kb 3 3, q 3, dodecahedron, G.to1n heawoodGraph, G.to1n coxeterGraph, G.to1n tutteCoxeterGraph] ++
    map is2ArcTransitive [c 7, q 3, G.to1n coxeterGraph] ++
    map is3ArcTransitive [c 7, G.to1n petersen] ++
    map (not . is3ArcTransitive) [q 3] ++
    -- [isArcTransitive (j v k i) | v <- [3..5], k <- [1..v `div` 2], i <- [0..k] ] ++ -- [AGT] p60
    -- !! j 4 2 0 is not connected, so this test now gives error. Not sure how it passed before
    [is2ArcTransitive (j (2*k+1) k 0) | k <- [1..2] ] ++
    [isDistanceTransitive (j v k (k-1)) | v <- [3..5], k <- [1..v `div` 2] ] ++ -- [AGT] p75
    [isDistanceTransitive (j (2*k+1) k 0) | k <- [1..2] ] ++
    [p doyleGraph | p <- [isVertexTransitive, isEdgeTransitive, not . isArcTransitive, not . isDistanceTransitive] ]

-- Most of the graphs we construct are highly symmetric, and turn out to be arc- and distance-transitive
-- On the other hand, those which aren't arc- or distance-transitive are often trivially not so,
-- by virtue of not even being vertex- or edge-transitive
-- It is actually rather hard to find graphs which are vertex- and edge-transitive but not arc-transitive, but here is one
-- Doyle, "A 27-vertex graph that is vertex-transitive and edge-transitive but not 1-transitive"
-- http://en.wikipedia.org/wiki/Holt_graph
doyleGraph = cayleyGraphS (['a','c'],
    [("aaaaaaaaa",""), ("ccccccccc",""), ("aaaaaa","ccc"), ("cccccc","aaa"), ("ccccccccac","aaaa"), ("aaaaaaaaca","cccc")])
-- so the vertices are the elts g, and the edges join g to ga, gc, ga^-1, gc^-1


srgParamTest = all (uncurry (==)) srgParamTests

-- van Lint & Wilson 262
srgParamTests =
    [(srgParams $ SRG.t m, Just (m `choose` 2, 2*(m-2), m-2, 4) ) | m <- [4..7] ]
    ++ [(srgParams $ l2 m, Just (m^2, 2*(m-1), m-2, 2) ) | m <- [2..6] ]
--     ++ [(srgParams $ paleyGraph fq, Just (q, (q-1) `div` 2, (q-5) `div` 4, (q-1) `div` 4) ) | (q,fq) <- [(5,f5),(9,f9),(13,f13),(17,f17)] ]
    ++ [(srgParams $ G.petersen, Just (10,3,0,1) ) ]
    ++ [(srgParams $ clebsch, Just (16,5,0,2) ) ]
    ++ [(srgParams $ hoffmanSingleton, Just (50,7,0,1) ) ]
    ++ [(srgParams $ higmanSimsGraph, Just (100,22,0,6) ) ]
    ++ [(srgParams $ sp (2*r), Just (2^(2*r)-1,2^(2*r-1),2^(2*r-2),2^(2*r-2))) | r <- [2..3] ]


graphAutTest = all (uncurry (==)) graphAutTests

graphAutTests =
    [(SS.order $ graphAuts $ c n, 2*n) | n <- [3..6] ] -- Aut(C n) = _D2 n
    ++ [(SS.order $ graphAuts $ k n, factorial n) | n <- [3..6] ] -- Aut(K n) = S n
    ++ [(SS.order $ graphAuts $ kb m n, factorial m * factorial n) | m <- [1..4], n <- [m+1..5] ] -- Aut(K m n) = S m * S n (m /= n)
    ++ [(SS.order $ graphAuts $ kb n n, 2 * (factorial n)^2 ) | n <- [1..5] ] -- Aut(K n n) = S n * S n * C2 (m == n)
    ++ [(SS.order $ graphAuts $ l2 n, 2 * (factorial n)^2 ) | n <- [2..5] ] -- Aut(L2 n) = S m * S m * C2

