-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCommutativeAlgebra.TPolynomial where

import Test.HUnit

import Data.List as L
import Math.Core.Field
-- import Math.Algebras.VectorSpace
import Math.CommutativeAlgebra.Polynomial


testlistPolynomial = TestList [
    testlistMonomialOrders1,
    testlistMonomialOrders2,
    testlistOverFiniteField,
    testlistEval,
    testlistSubst
    ]


testcaseMonomialOrder1 desc monomials expected =
    TestCase $ assertEqual desc expected (L.sort monomials)

testlistMonomialOrders1 = TestList [
   let [x,y,z] = map lexvar ["x","y","z"]
   in testcaseMonomialOrder1 "Lex" [1,x,y,z,x^2,x*y,x*z,y^2,y*z,z^2] [x^2,x*y,x*z,x,y^2,y*z,y,z^2,z,1],
   let [x,y,z] = map glexvar ["x","y","z"]
   in testcaseMonomialOrder1 "Glex" [1,x,y,z,x^2,x*y,x*z,y^2,y*z,z^2] [x^2,x*y,x*z,y^2,y*z,z^2,x,y,z,1],
   let [x,y,z] = map grevlexvar ["x","y","z"]
   in testcaseMonomialOrder1 "Grevlex" [1,x,y,z,x^2,x*y,x*z,y^2,y*z,z^2] [x^2,x*y,y^2,x*z,y*z,z^2,x,y,z,1]
   ]


testcaseMonomialOrder2 desc poly string =
    TestCase $ assertEqual desc string (show poly)

testlistMonomialOrders2 = TestList [
    let [x,y,z] = map lexvar ["x","y","z"]
    in testcaseMonomialOrder2 "Lex" ((x+y+z+1)^3)
       "x^3+3x^2y+3x^2z+3x^2+3xy^2+6xyz+6xy+3xz^2+6xz+3x+y^3+3y^2z+3y^2+3yz^2+6yz+3y+z^3+3z^2+3z+1",
    let [x,y,z] = map glexvar ["x","y","z"]
    in testcaseMonomialOrder2 "Glex" ((x+y+z+1)^3)
       "x^3+3x^2y+3x^2z+3xy^2+6xyz+3xz^2+y^3+3y^2z+3yz^2+z^3+3x^2+6xy+6xz+3y^2+6yz+3z^2+3x+3y+3z+1",
    let [x,y,z] = map grevlexvar ["x","y","z"]
    in testcaseMonomialOrder2 "Grevlex" ((x+y+z+1)^3)
       "x^3+3x^2y+3xy^2+y^3+3x^2z+6xyz+3y^2z+3xz^2+3yz^2+z^3+3x^2+6xy+3y^2+6xz+6yz+3z^2+3x+3y+3z+1"
    ]


testcaseOverFiniteField desc input expected = TestCase $ assertEqual desc expected input

testlistOverFiniteField = TestList [
    let [x,y,z] = map var ["x","y","z"] :: [GlexPoly F3 String] in
    testcaseOverFiniteField "F3" ((x+y+z)^3) (x^3+y^3+z^3),
    let [x,y,z] = map var ["x","y","z"] :: [GlexPoly F5 String] in
    testcaseOverFiniteField "F5" ((x+y+z)^5) (x^5+y^5+z^5)
    ]


testcaseEval poly point value =
    TestCase $ assertEqual "Eval" value (eval poly point)

testlistEval =
    let [x,y,z] = map glexvar ["x","y","z"] in TestList [
        testcaseEval (x^2+y^2-z^2) [(x,3),(y,4),(z,5)] 0,
        testcaseEval (z-1) [(x,100),(y,-100),(z,1)] 0
        ]


testcaseSubst poly subs poly' =
    TestCase $ assertEqual "Subst" poly' (subst poly subs)

testlistSubst =
    let [x,y,z,x',y',z'] = map glexvar ["x","y","z","x'","y'","z'"] in TestList [
        testcaseSubst (x^2+y^2-z^2) [(x,(x'-z')/2),(y,y'),(z,(x'+z')/2)] (y'^2-x'*z')
        ]


-- The division algorithm is adequately tested by the GroebnerBasis tests
