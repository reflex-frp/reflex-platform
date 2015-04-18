-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCombinatorics.TMatroid where

import Test.HUnit

import Math.Combinatorics.Matroid

import Math.Core.Field hiding (f7)
-- import Math.Algebra.Field.Base hiding (f7)
import Math.Core.Utils (combinationsOf)

testlistMatroid = TestList [
    testlistKnownEquivalent,
    testlistFromCircuits,
    testlistFromBases,
    testlistFromRankfun,
    testlistFromClosure,
    testlistFromFlats,
    testlistFromHyperplanes,
    testlistRepresentable
    ]


elts = elements


-- Oxley p8
ex112 = vectorMatroid' [[1,0],[0,1],[0,0],[1,0],[1,1::Q]]

-- Oxley p11 - leads to same matroid as ex112
ex118 = cycleMatroid [ [1,2],[1,3],[3,3],[1,2],[2,3]]

-- Oxley p19 - two non-isomorphic graphs giving rise to the same matroid
fig13a = cycleMatroid [ [1,1],[2,3],[2,4],[2,5],[6,7]]

fig13b = cycleMatroid [ [1,1],[1,2],[2,3],[3,4],[4,5]]

-- Oxley p34, fig 1.8
ex154 = fromGeoRep [[3]] [[1,4]] [[1,2,5],[2,4,5]] [[1..5]]

-- Oxley p46
ex163a = cycleMatroid [ [1,2],[1,2],[2,3],[2,3],[3,4],[3,4],[1,4] ]
ex163b = transversalMatroid [1..7] [ [1,2,7],[3,4,7],[5,6,7] ]


{-
-- not currently used
ex16 = affineMatroid [ [0,0],[0,1],[0,2],[1,0],[1,1],[2,0::Q]]

ex17 = affineMatroid [ [0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,1,0::Q]]

-}



testcaseEquivalent desc m1 m2 = TestCase (assertEqual desc m1 m2)

testlistKnownEquivalent = TestList [
    testcaseEquivalent "ex112=118" ex112 ex118,
    testcaseEquivalent "fig13a=fig13b" fig13a fig13b,
    testcaseEquivalent "ex112=ex154" ex112 ex154,
    testcaseEquivalent "desargues = M(K5)" desargues (cycleMatroid (combinationsOf 2 [1..5])), -- Oxley p39
    testcaseEquivalent "ex163a=ex163b" ex163a ex163b
    ]


exampleList = [
    ("ex112", ex112),
    ("fig13a", fig13a),
    ("f7", f7),
    ("pappus",pappus),
    ("nonPappus",nonPappus),
    ("desargues",desargues),
    ("v8",v8)
    ]
-- if we're really just going to use the same examples for each test, should use map

testcaseFromCircuits (desc, m) = TestCase (assertEqual ("fromCircuits " ++ desc) m (fromCircuits (elts m) (circuits m)))

testlistFromCircuits = TestList $ map testcaseFromCircuits exampleList


testcaseFromBases (desc, m) = TestCase (assertEqual ("fromBases " ++ desc) m (fromBases (elts m) (bases m)))

testlistFromBases = TestList $ map testcaseFromBases exampleList


testcaseFromRankfun (desc, m) = TestCase (assertEqual ("fromRankfun " ++ desc) m (fromRankfun (elts m) (rankfun m)))

testlistFromRankfun = TestList $ map testcaseFromRankfun exampleList

 
testcaseFromClosure (desc, m) = TestCase (assertEqual ("fromClosure " ++ desc) m (fromClosure (elts m) (closure m)))

testlistFromClosure = TestList $ map testcaseFromClosure exampleList


testcaseFromFlats (desc, m) = TestCase (assertEqual ("fromFlats " ++ desc) m (fromFlats (flats m)))

testlistFromFlats = TestList $ map testcaseFromFlats exampleList


testcaseFromHyperplanes (desc, m) = TestCase (assertEqual ("fromHyperplanes " ++ desc) m (fromHyperplanes (elts m) (hyperplanes m)))

testlistFromHyperplanes = TestList $ map testcaseFromHyperplanes exampleList


testcaseIsRepresentable desc fq m = TestCase (assertBool ("isRepresentable " ++ desc) (isRepresentable fq m))
testcaseNotRepresentable desc fq m = TestCase (assertBool ("notRepresentable " ++ desc) (not (isRepresentable fq m)))

testlistRepresentable = TestList [
    testcaseNotRepresentable "f2 v8" f2 v8, -- Oxley p84
    testcaseNotRepresentable "f3 v8" f3 v8,
    testcaseNotRepresentable "f4 v8" f4 v8,
    testcaseNotRepresentable "f5 v8" f5 v8,

    testcaseIsRepresentable "f2 f7" f2 f7, -- Oxley p187
    testcaseNotRepresentable "f3 f7" f3 f7,
    testcaseIsRepresentable "f4 f7" f4 f7,
    testcaseNotRepresentable "f5 f7" f5 f7,
    testcaseNotRepresentable "f2 f7m" f2 f7m,
    testcaseIsRepresentable "f3 f7m" f3 f7m,
    testcaseNotRepresentable "f4 f7m" f4 f7m,
    testcaseIsRepresentable "f5 f7m" f5 f7m,

    testcaseNotRepresentable "f2 p8" f2 p8, -- Oxley p189-90
    testcaseIsRepresentable "f3 p8" f3 p8,
    testcaseNotRepresentable "f4 p8" f4 p8,
    testcaseIsRepresentable "f5 p8" f5 p8,
    testcaseNotRepresentable "f2 p8m" f2 p8m,
    testcaseNotRepresentable "f3 p8m" f3 p8m,
    testcaseIsRepresentable "f4 p8m" f4 p8m,
    testcaseIsRepresentable "f5 p8m" f5 p8m,
    testcaseNotRepresentable "f2 p8mm" f2 p8mm,
    testcaseNotRepresentable "f3 p8mm" f3 p8mm,
    testcaseNotRepresentable "f4 p8mm" f4 p8mm,
    testcaseIsRepresentable "f5 p8mm" f5 p8mm,

    testcaseNotRepresentable "f2 u24" f2 (u 2 4), -- Oxley p193
    testcaseNotRepresentable "f3 u25" f3 (u 2 5),
    testcaseNotRepresentable "f3 u35" f3 (u 3 5),
    testcaseNotRepresentable "f4 u26" f4 (u 2 6),
    testcaseNotRepresentable "f4 u46" f4 (u 4 6),
    testcaseNotRepresentable "f5 u27" f5 (u 2 7),
    testcaseNotRepresentable "f5 u57" f5 (u 5 7)
    -- Oxley does mention other excluded minors for other fields, but we have to stop somewhere
    ]
