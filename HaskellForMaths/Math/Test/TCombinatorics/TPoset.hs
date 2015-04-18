-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCombinatorics.TPoset where

import Test.HUnit

import Math.Combinatorics.Digraph
import Math.Combinatorics.Poset

testlistPoset = TestList [
    testlistIsOrderIsoPositive,
    testlistIsOrderIsoNegative,
    testlistHasseDigraph
    ]



testcaseIsOrderIsoPositive desc p1 p2 = TestCase (assertBool desc (isOrderIso p1 p2))

testlistIsOrderIsoPositive = TestList [
    testcaseIsOrderIsoPositive "D 30 ~= D 42" (posetD 30) (posetD 42),
    testcaseIsOrderIsoPositive "D 60 ~= D 90" (posetD 30) (posetD 42),
    testcaseIsOrderIsoPositive "D 30 ~= B 3" (posetD 30) (posetB 3),
    testcaseIsOrderIsoPositive "B 2 ~= 2 * 2" (posetB 2) (dprod (chainN 2) (chainN 2))
    ]


testcaseIsOrderIsoNegative desc p1 p2 = TestCase (assertBool desc (not (isOrderIso p1 p2)))

testlistIsOrderIsoNegative = TestList [
    testcaseIsOrderIsoNegative "D 20 ~/= D 30" (posetD 20) (posetD 30)
    ]


testcaseHasseDigraph desc poset = TestCase $ assertEqual desc poset (reachabilityPoset $ hasseDigraph poset)

testlistHasseDigraph = TestList [
    testcaseHasseDigraph "chain 3" (chainN 3),
    testcaseHasseDigraph "antichain 3" (antichainN 3),
    testcaseHasseDigraph "posetB 3" (posetB 3),
    testcaseHasseDigraph "posetB 4" (posetB 4)
    ]
