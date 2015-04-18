-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCombinatorics.TGraphAuts where

import Data.List as L
import Math.Core.Field hiding (f7)
import Math.Core.Utils (combinationsOf)
import Math.Algebra.Group.PermutationGroup as P
import Math.Combinatorics.Graph as G
import Math.Combinatorics.GraphAuts
import Math.Combinatorics.Matroid as M

import Test.HUnit


testlistGraphAuts = TestList [
    testlistGraphAutsOrder,
    testlistGraphAutsGroup,
    testlistGraphAutsComplement,
    testlistIncidenceAutsOrder,
    testlistGraphIsos,
    testlistIsGraphIso,
    testlistIncidenceIsos
    ]


-- We know the expected order of the graph automorphism group
testcaseGraphAutsOrder desc g n = TestCase $
    assertEqual ("order " ++ desc) n (orderSGS $ graphAuts g)

testlistGraphAutsOrder = TestList [
    let g = G [1..6] [[1,2],[3,4],[5,6]] in
        testcaseGraphAutsOrder (show g) g 48, -- 2*2*2*3!
    testcaseGraphAutsOrder "cube" cube 48,
    testcaseGraphAutsOrder "dodecahedron" dodecahedron 120
    ]


induced bs g = fromPairs [(b, b -^ g) | b <- bs]

-- We know the expected group of graph automorphisms
testcaseGraphAutsGroup desc graph group = TestCase $
    assertEqual ("group " ++ desc) (elts $ graphAuts graph) (elts $ group)

testlistGraphAutsGroup = TestList [
    testcaseGraphAutsGroup "nullGraph 0" (nullGraph 0) [],
    testcaseGraphAutsGroup "nullGraph 1" (nullGraph 1) (_S 1),
    testcaseGraphAutsGroup "nullGraph 2" (nullGraph 2) (_S 2),
    testcaseGraphAutsGroup "nullGraph 3" (nullGraph 3) (_S 3),
    testcaseGraphAutsGroup "k 3" (k 3) (_S 3),
    testcaseGraphAutsGroup "k 4" (k 4) (_S 4),
    testcaseGraphAutsGroup "k 5" (k 5) (_S 5),
    testcaseGraphAutsGroup "c 4" (c 4) (_D 8),
    testcaseGraphAutsGroup "c 5" (c 5) (_D 10),
    let graph = G [1..3] [[1,2],[2,3]] in
        testcaseGraphAutsGroup (show graph) graph [p [[1,3]]], -- regression test
    let graph = G [1..6] [[2,3],[4,5],[5,6]] in
        testcaseGraphAutsGroup (show graph) graph [p [[2,3]], p [[4,6]]],
    testcaseGraphAutsGroup "petersen" petersen (map (induced $ combinationsOf 2 [1..5]) $ _S 5)
    ]


-- The automorphisms of the graph should be the same as the auts of its complement
testcaseGraphAutsComplement desc g = TestCase $
    assertEqual ("complement " ++ desc) (elts $ graphAuts g) (elts $ graphAuts $ complement g)
-- the algorithm may not find the same set of generators, so we have to compare the elements

testlistGraphAutsComplement = TestList [
    testcaseGraphAutsComplement "k 3" (k 3),
    testcaseGraphAutsComplement "kb 2 3" (kb 2 3), -- complement is not connected
    testcaseGraphAutsComplement "kb 3 3" (kb 3 3), -- complement is not connected, but components can be swapped
    testcaseGraphAutsComplement "kt 2 3 3" (kt 2 3 3),
    testcaseGraphAutsComplement "kt 2 3 4" (kt 2 3 4),
    testcaseGraphAutsComplement "kt 3 3 3" (kt 3 3 3)
    ]

kt a b c = graph (vs,es)
    where vs = [1..a+b+c]
          es = L.sort $ [[i,j] | i <- [1..a], j <- [a+1..a+b] ]
                     ++ [[i,k] | i <- [1..a], k <- [a+b+1..a+b+c] ]
                     ++ [[j,k] | j <- [a+1..a+b], k <- [a+b+1..a+b+c] ]

-- We know the expected order of the incidence structure automorphism group
testcaseIncidenceAutsOrder desc g n = TestCase $
    assertEqual ("incidence order " ++ desc) n (P.order $ incidenceAuts g)

-- We use matroids as our incidence structure just because we have a powerful library for constructing them
testlistIncidenceAutsOrder = TestList [
    testcaseIncidenceAutsOrder "pg2 f2 (B)" (incidenceGraphB $ matroidPG 2 f2) 168,
    testcaseIncidenceAutsOrder "pg2 f2 (C)" (incidenceGraphC $ matroidPG 2 f2) 168,
    testcaseIncidenceAutsOrder "pg2 f2 (H)" (incidenceGraphH $ matroidPG 2 f2) 168,
    testcaseIncidenceAutsOrder "u 1 3 (B)" (incidenceGraphB $ u 1 3) 6, -- not connected
    testcaseIncidenceAutsOrder "u 1 3 (C)" (incidenceGraphC $ u 1 3) 6,
    testcaseIncidenceAutsOrder "u 1 3 (H)" (incidenceGraphH $ u 1 3) 6, -- not connected
    testcaseIncidenceAutsOrder "u 2 3 `dsum` u 2 3 (H)" (incidenceGraphH $ u 2 3 `dsum` u 2 3) 72, -- 6*6*2
    testcaseIncidenceAutsOrder "u 2 3 `dsum` u 2 3 (C)" (incidenceGraphC $ u 2 3 `dsum` u 2 3) 72, -- not connected
    testcaseIncidenceAutsOrder "u 2 3 `dsum` u 3 4 (H)" (incidenceGraphH $ u 2 3 `dsum` u 3 4) 144, -- 6*24
    testcaseIncidenceAutsOrder "u 2 3 `dsum` u 3 4 (C)" (incidenceGraphC $ u 2 3 `dsum` u 3 4) 144 -- not connected
    ]


testcaseGraphIsos g1 g2 isos = TestCase $
    assertEqual (show (g1,g2)) isos (graphIsos g1 g2)

testlistGraphIsos = TestList [
    testcaseGraphIsos (G [1,2] []) (G [3,4] []) [[(1,3),(2,4)],[(1,4),(2,3)]],
    testcaseGraphIsos (G [1,2,3] [[1,2]]) (G [4,5,6] [[5,6]]) [[(1,5),(2,6),(3,4)],[(1,6),(2,5),(3,4)]]
    ]


testcaseIsGraphIso g1 g2 = TestCase $
    assertBool (show (g1,g2)) $ isGraphIso g1 g2

testlistIsGraphIso = TestList [
    testcaseIsGraphIso (nullGraph') (nullGraph')
    ]


testcaseIncidenceIsos g1 g2 isos = TestCase $
    assertEqual (show (g1,g2)) isos (incidenceIsos g1 g2)

testlistIncidenceIsos = TestList [
    testcaseIncidenceIsos (G [Left 1, Right 2] []) (G [Left 3, Right 4] []) [[(1,3)]],
    testcaseIncidenceIsos (G [Left 1, Left 2, Right 1] [[Left 1, Right 1]])
                          (G [Left 3, Left 4, Right 4] [[Left 4, Right 4]])
                          [[(1,4),(2,3)]]
    ]
