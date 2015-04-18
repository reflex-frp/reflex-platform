-- Copyright (c) David Amos, 2009. All rights reserved.

module Math.Test.TSubquotients where

import Math.Algebra.Group.PermutationGroup hiding (ptStab, normalClosure)
-- import Math.Algebra.Group.SchreierSims (cosetRepsGx)
-- import Math.Algebra.Group.RandomSchreierSims

import Math.Algebra.Group.Subquotients

import qualified Math.Algebra.Group.PermutationGroup as P -- for testing


test = and [testPtStab, testTransitiveConstituentHomomorphism, testBlockSystems, testBlockHomomorphism,
                  testNormalClosure, testCentralizerSymTrans]

-- TESTS

testPtStab =
    let gs = [p [[1..5],[6,7]], p [[1,2],[6..10]] ] -- S 5 * S 5
        gs16 = ptStab gs [1,6]
    in orderSGS gs16 == 24*24 && orbitP gs16 1 == [1] && orbitP gs16 6 == [6]

testTransitiveConstituentHomomorphism =
    let gs1 = [p [[1..5],[6,7]], p [[1,2],[6..10]] ] -- S 5 * S 5
        (ker1,im1) = transitiveConstituentHomomorphism gs1 [1..5]
        gs2 = [p [[1,2,3],[4,5,6]], p [[1,2],[4,5]]] -- note that the two halves don't move independently
        (ker2,im2) = transitiveConstituentHomomorphism gs2 [1,2,3]
        gs3 = [p [[1,2],[4,5]], p [[1,2,3]], p [[4,5,6]] ]
        -- so we can achieve odd permutations in each half, but we can't achieve an odd in one without achieving an odd in the other
        -- hence the order of the group is only half the order of the left image * the order of the right image
        (ker3,im3) = transitiveConstituentHomomorphism gs3 [1,2,3]
    in orderSGS ker1 == 120 && orderSGS im1 == 120 &&
       null ker2 && orderSGS im2 == 6 &&
       orderSGS im3 == 6 && orderSGS ker3 == 3


testBlockSystems =
    blockSystems [p [[1..3]], p [[4..6]], p [[1,4],[2,5],[3,6]]] == [ [[1,2,3],[4,5,6]] ] &&
    blockSystems (_D 10) == [] &&
    blockSystems (_D 12) == [ [[1,3,5],[2,4,6]], [[1,4],[2,5],[3,6]] ]

testBlockHomomorphism =
    let (ker14,im14) = blockHomomorphism (_D 12) [[1,4],[2,5],[3,6]]
        (ker135,im135) = blockHomomorphism (_D 12) [[1,3,5],[2,4,6]]
    in (orderSGS ker14, orderSGS im14) == (2,6) &&
       (orderSGS ker135, orderSGS im135) == (6,2)

-- !! Need to improve this test
testNormalClosure =
    let gs = [p [[1,4]]]
        hs = [p [[1,2,3]]]
    in elts (P.normalClosure gs hs) == elts (normalClosure gs hs)
-- == _A 4


testCentralizerSymTrans =
    let gs = [ p [[1..5]] ]
        hs = [ p [[1,2],[3,4]], p [[1,3],[2,4]] ]
    in centralizerSymTrans gs == gs
    && centralizerSymTrans hs == hs