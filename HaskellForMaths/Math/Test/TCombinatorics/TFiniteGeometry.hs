-- Copyright (c) 2011, David Amos. All rights reserved

module Math.Test.TCombinatorics.TFiniteGeometry where

import Test.HUnit

import Math.Combinatorics.FiniteGeometry
import Math.Core.Field
import Math.Combinatorics.GraphAuts (incidenceAuts)
import Math.Algebra.Group.PermutationGroup (orderSGS)
import Math.NumberTheory.Factor (pfactors)

testlistFiniteGeometry = TestList [
    testlistFlatsAG,
    testlistFlatsPG,
    testlistAutsAG,
    testlistAutsPG
    ]


-- !! can't make list [f2,f3,f4], because they're different types
testcaseFlatsAG n fq k = TestCase $
    assertEqual (show "flatsAG " ++ show n ++ " " ++ show q ++ " " ++ show k)
                (numFlatsAG n q k) (length (flatsAG n fq k))
    where q = length fq

testlistFlatsAG = TestList $
    [testcaseFlatsAG n f2 k | n <- [2,3], k <- [0..n]] ++
    [testcaseFlatsAG n f3 k | n <- [2,3], k <- [0..n]] ++
    [testcaseFlatsAG n f4 k | n <- [2,3], k <- [0..n]]


testcaseFlatsPG n fq k = TestCase $
    assertEqual (show "flatsPG " ++ show n ++ " " ++ show q ++ " " ++ show k)
                (numFlatsPG n q k) (length (flatsPG n fq k))
    where q = length fq

testlistFlatsPG = TestList $
    [testcaseFlatsPG n f2 k | n <- [2,3], k <- [0..n]] ++
    [testcaseFlatsPG n f3 k | n <- [2,3], k <- [0..n]] ++
    [testcaseFlatsPG n f4 k | n <- [2,3], k <- [0..n]]


testcaseAutsAG n fq = TestCase $
    assertEqual ("autsAG " ++ show n ++ " " ++ show q)
                (orderAff n q * degree)
                (orderSGS $ incidenceAuts $ incidenceGraphAG n fq)
    where q = toInteger $ length fq
          degree = toInteger $ length $ pfactors $ toInteger q

testlistAutsAG = TestList $ 
    -- [testcaseAutsAG n f2 | n <- [2,3] ] ++ -- this is the complete graph, so has more auts than expected
    [testcaseAutsAG n f3 | n <- [2] ] -- ++
    -- [testcaseAutsAG n f4 | n <- [2,3] ] -- these take too long


testcaseAutsPG n fq = TestCase $
    assertEqual ("autsPG " ++ show n ++ " " ++ show q)
                (orderPGL (n+1) q * degree)
                (orderSGS $ incidenceAuts $ incidenceGraphPG n fq)
    where q = toInteger $ length fq
          degree = toInteger $ length $ pfactors $ toInteger q

testlistAutsPG = TestList  $ 
    [testcaseAutsPG n f2 | n <- [2,3] ] ++
    [testcaseAutsPG n f3 | n <- [2] ] -- ++
    -- [testcaseAutsPG n f4 | n <- [2,3] ] -- these take too long
