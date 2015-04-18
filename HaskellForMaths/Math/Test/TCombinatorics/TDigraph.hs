-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCombinatorics.TDigraph where

import Test.HUnit
import Control.Monad (when, unless)

import Math.Core.Utils (pairs)
import Math.Combinatorics.Digraph
import Math.Combinatorics.Poset


testlistDigraph = TestList [
    testlistIsDagIsoPositive,
    testlistIsDagIsoNegative,
    testlistIsoRepDAGIsIso,
    testlistIsoRepDAGPositive,
    testlistIsoRepDAGNegative
    ]


testcaseIsDagIsoPositive desc dag1 dag2 = TestCase $ assertBool desc $ isDagIso dag1 dag2

testlistIsDagIsoPositive = TestList [
    testcaseIsDagIsoPositive "D 30 ~= D 42" (hasseDigraph $ posetD 30) (hasseDigraph $ posetD 42),
    testcaseIsDagIsoPositive "D 60 ~= D 90" (hasseDigraph $ posetD 30) (hasseDigraph $ posetD 42),
    testcaseIsDagIsoPositive "D 30 ~= B 3" (hasseDigraph $ posetD 30) (hasseDigraph $ posetB 3),
    testcaseIsDagIsoPositive "B 2 ~= 2 * 2" (hasseDigraph $ posetB 2) (hasseDigraph $ dprod (chainN 2) (chainN 2))
    ]

testcaseIsDagIsoNegative desc dag1 dag2 = TestCase $ assertBool desc $ not (isDagIso dag1 dag2)

testlistIsDagIsoNegative = TestList [
    testcaseIsDagIsoNegative "D 20 ~/= D 30" (hasseDigraph $ posetD 20) (hasseDigraph $ posetD 30),
    testcaseIsDagIsoNegative "Subposets B4 - 1" (hasseDigraph $ subposet (posetB 4) (/= [1]))
                                                (hasseDigraph $ subposet (posetB 4) (/= [1,2])),
    testcaseIsDagIsoNegative "Subposets B4 - 2" (hasseDigraph $ subposet (posetB 4) (`notElem` [[1],[1,2]]))
                                                (hasseDigraph $ subposet (posetB 4) (`notElem` [[1],[2,3]]))
    ]


-- test that the isoRepDAG is isomorphic to the DAG
testcaseIsoRepDAGIsIso desc dag = TestCase $ assertBool desc $ isDagIso dag (isoRepDAG dag)

testlistIsoRepDAGIsIso = TestList [
    testcaseIsoRepDAGIsIso "D 30" (hasseDigraph $ posetD 30),
    testcaseIsoRepDAGIsIso "B 4 - [1,2]" (hasseDigraph $ subposet (posetB 4) (/= [1,2]))
    ]


testcaseIsoRepDAGPositive desc dag1 dag2 = TestCase (assertEqual desc (isoRepDAG dag1) (isoRepDAG dag2))

testlistIsoRepDAGPositive = TestList [
    testcaseIsoRepDAGPositive "D 30 ~= D 42" (hasseDigraph $ posetD 30) (hasseDigraph $ posetD 42),
    testcaseIsoRepDAGPositive "D 60 ~= D 90" (hasseDigraph $ posetD 30) (hasseDigraph $ posetD 42),
    testcaseIsoRepDAGPositive "D 30 ~= B 3" (hasseDigraph $ posetD 30) (hasseDigraph $ posetB 3),
    testcaseIsoRepDAGPositive "B 2 ~= 2 * 2" (hasseDigraph $ posetB 2) (hasseDigraph $ dprod (chainN 2) (chainN 2))
    ] 


assertNotEqual desc val1 val2 =
    when (val1 == val2) (assertFailure desc)
    -- unless (val1 /= val2) (assertFailure desc)

testcaseIsoRepDAGNegative desc dag1 dag2 = TestCase (assertNotEqual desc (isoRepDAG dag1) (isoRepDAG dag2))

testlistIsoRepDAGNegative = TestList [
    testcaseIsoRepDAGNegative "Subposets B4 - 1" (hasseDigraph $ subposet (posetB 4) (/= [1]))
                                                 (hasseDigraph $ subposet (posetB 4) (/= [1,2])),
    testcaseIsoRepDAGNegative "Subposets B4 - 2" (hasseDigraph $ subposet (posetB 4) (`notElem` [[1],[1,2]]))
                                                 (hasseDigraph $ subposet (posetB 4) (`notElem` [[1],[2,3]]))
    ] 



allDags n = [DG [1..n] es | es <- powerset (pairs [1..n])]

-- > all (uncurry (==)) [(dag1 `isDagIso` dag2, isoRepDAG dag1 == isoRepDAG dag2) | (dag1,dag2) <- pairs (allDags 4)]

{-
-- Following tests no longer valid, as isoRepDAG doesn't produce same representative as isoRepDAG1
testcaseIsoRepDAG desc dag = TestCase (assertEqual desc (isoRepDAG1 dag) (isoRepDAG dag))

testlistIsoRepDAG = TestList [
    testcaseIsoRepDAG "posetB 3" (hasseDigraph (posetB 3)),
    testcaseIsoRepDAG "posetP 3" (hasseDigraph (posetP 3)),
    testcaseIsoRepDAG "dual (chainN 5)" (hasseDigraph (dual (chainN 5))),
    testcaseIsoRepDAG "antiChainN 5" (hasseDigraph (antichainN 5)),
    testcaseIsoRepDAG "posetD 60" (hasseDigraph (posetD 60)),
    testcaseIsoRepDAG "dprod (posetB 2) (chainN 3)" (hasseDigraph (dprod (posetB 2) (chainN 3))),
    testcaseIsoRepDAG "DG ['a'..'e'] [('a','d'),('a','e'),('b','c'),('b','d'),('d','e')]"
        (DG ['a'..'e'] [('a','d'),('a','e'),('b','c'),('b','d'),('d','e')])
    ] 
-}
