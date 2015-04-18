-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCombinatorics.TIncidenceAlgebra where

import Test.HUnit

import Math.Algebra.Field.Base

import Math.Combinatorics.Digraph
import Math.Combinatorics.Poset
import Math.Combinatorics.IncidenceAlgebra

testlistIncidenceAlgebra = TestList [
    testlistMuReference,
    testlistMuInverse
    ]



-- test that the calculated mu matches reference definition
testcaseMuReference desc poset muref = TestCase $ assertEqual desc muref (muIA poset)

testlistMuReference = TestList [
    testcaseMuReference "chainN 3" (chainN 3) (muC 3),
    testcaseMuReference "posetB 3" (posetB 3) (muB 3),
    testcaseMuReference "posetL 3 f3" (posetL 3 f3) (muL 3 f3)
    ] 


-- test that muIA is multiplicative inverse of zetaIA
testcaseMuInverse desc poset = TestCase (assertEqual desc (unitIA poset) (muIA poset * zetaIA poset))

testlistMuInverse = TestList [
    testcaseMuInverse "chainN 3" (chainN 3),
    testcaseMuInverse "antichainN 3" (antichainN 3),
    testcaseMuInverse "posetB 3" (posetB 3),
    testcaseMuInverse "posetP 3" (posetP 3)
    ] 
