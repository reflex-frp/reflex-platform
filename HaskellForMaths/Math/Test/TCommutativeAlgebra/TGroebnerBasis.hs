-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCommutativeAlgebra.TGroebnerBasis where

import Test.HUnit

import Math.Core.Field
import Math.Algebras.VectorSpace
import Math.CommutativeAlgebra.Polynomial
import Math.CommutativeAlgebra.GroebnerBasis

-- Sources:
-- Eisenbud, Commutative Algebra with a View Toward Algebraic Geometry
-- [IVA] - Cox, Little, O'Shea: Ideals, Varieties and Algorithms, 2nd ed
-- (Note that I have the 5th printing, which is known to have some misprints)
-- [UAG] - Cox, Little, O'Shea: Using Algebraic Geometry
-- Schenck, Computational Algebraic Geometry

testlistGroebnerBasis = TestList [
    testlistLexGb,
    testlistGlexGb,
    testlistIntersectI,
    testlistQuotientI,
    testlistEliminate,
    -- testlistElimExcept,
    testlistHilbertPolyQA
    ]


data Var = X | Y | Z | W deriving (Eq,Ord)

instance Show Var where
    show X = "x"
    show Y = "y"
    show Z = "z"
    show W = "w"

[x,y,z,w] = map grevlexvar [X,Y,Z,W]


testcaseGb desc input output = TestCase (assertEqual desc output (gb input))

testlistLexGb =
    let [x,y,z] = map lexvar ["x","y","z"] in
    TestList [
        testcaseGb "Lex <x^2, xy+y^2>" [x^2,x*y+y^2] [x^2,x*y+y^2,y^3], -- Eisenbud p339
        testcaseGb "Lex <x^2+y^2+z^2-1,x^2+z^2-y,x-z>" [x^2+y^2+z^2-1,x^2+z^2-y,x-z] [x-z,y-2*z^2,z^4+1/2*z^2-1/4], -- IVA p93-4
        testcaseGb "Lex <x^2+y^2+z^2-1,x*y*z-1>" [x^2+y^2+z^2-1,x*y*z-1] [x+y^3*z+y*z^3-y*z,y^4*z^2+y^2*z^4-y^2*z^2+1], -- IVA p116
        testcaseGb "Lex <x*y-4,y^2-(x^3-1)>" [x*y-4,y^2-(x^3-1)] [x-1/16*y^4-1/16*y^2,y^5+y^3-64] -- IVA p117, misprint corrected
    ]

testlistGlexGb =
    let [x,y,z] = map glexvar ["x","y","z"] in
    TestList [
        testcaseGb "Glex <x*z-y^2,x^3-z^2>" [x*z-y^2,x^3-z^2] [y^6-z^5,x*y^4-z^4,x^2*y^2-z^3,x^3-z^2,x*z-y^2], -- IVA p93
        testcaseGb "Glex <x^2+y,x*y+x>" [x^2+y,x*y+x] [x^2+y,x*y+x,y^2+y] -- Schenck p54
    ]


testcaseIntersectI desc i1 i2 iout =
    TestCase (assertEqual ("intersectI " ++ desc) iout (intersectI i1 i2))

testlistIntersectI =
    let [x,y] = map grevlexvar ["x","y"] in
    TestList [
        testcaseIntersectI "[x^2*y] [x*y^2]" [x^2*y] [x*y^2] [x^2*y^2], -- IVA p186
        testcaseIntersectI "IVA 186/2" [(x+y)^4*(x^2+y)^2*(x-5*y)] [(x+y)*(x^2+y)^3*(x+3*y)]
                                          [(x+y)^4*(x^2+y)^3*(x-5*y)*(x+3*y)] -- IVA p186
    ]

testcaseQuotientI desc i j q =
    TestCase (assertEqual ("quotientI " ++ desc) q (i `quotientI` j))

testlistQuotientI =
    let [x,y,z] = map grevlexvar ["x","y","z"] in
    TestList [
        testcaseQuotientI "[x*z, y*z] [z]" [x*z, y*z] [z] [x,y], -- IVA p192
        testcaseQuotientI "[y^2, z^2] [y*z]" [y^2, z^2] [y*z] [y,z] -- Schenck p56 (in passing)
    ]


testcaseEliminate vs gs gs' = TestCase $ assertEqual "Eliminate" gs' (eliminate vs gs)

testlistEliminate =
    let [t,u,v,x,y,z,x',y',z'] = map glexvar ["t","u","v","x","y","z","x'","y'","z'"] in
    TestList [
    testcaseEliminate [x,y,z] [x^2+y^2-z^2,x'-(x+z),y'-y,z'-(z-x)] [x'*z'-y'^2], -- Reid p15
    testcaseEliminate [t] [(t^2+1)*x-2*t, (t^2+1)*y-(t^2-1)] [x^2+y^2-1],
    testcaseEliminate [u,v] [x'-u^2,y'-u*v,z'-v^2] [x'*z'-y'^2] -- Reid p16
    ]

{-
testcaseElimExcept vs gs gs' = TestCase $ assertEqual "Eliminate" gs' (eliminate vs gs)

testlistElimExcept =
    let [t,u,v,x,y,z,x',y',z'] = map glexvar ["t","u","v","x","y","z","x'","y'","z'"] in
    TestList [
    testcaseElimExcept [x',y',z'] [x^2+y^2-z^2,x'-(x+z),y'-y,z'-(z-x)] [x'*z'-y'^2], -- Reid p15
    testcaseElimExcept [x,y] [(t^2+1)*x-2*t, (t^2+1)*y-(t^2-1)] [x^2+y^2-1],
    testcaseElimExcept [x',y',z'] [x'-u^2,y'-u*v,z'-v^2] [x'*z'-y'^2] -- Reid p16
    ]
-}

testcaseHilbertPolyQA desc hp vs gs =
    TestCase $ assertEqual desc hp (hilbertPolyQA vs gs)

testlistHilbertPolyQA =
    let i = glexvar "i" in
    TestList [
    testcaseHilbertPolyQA "hilbertPoly <yz-xw,z^2-yw,y^2-xz>" (3*i+1) [x,y,z,w] [y*z-x*w,z^2-y*w,y^2-x*z] -- Schenck p56-7
    ]


