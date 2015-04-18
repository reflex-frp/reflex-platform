-- Copyright (c) 2012, David Amos. All rights reserved.

-- {-# LANGUAGE #-}


module Math.Test.TNumberTheory.TQuadraticField where

import Prelude hiding (sqrt)

import Math.Core.Field
import Math.NumberTheory.QuadraticField

import Test.HUnit


testlistQuadraticField = TestList [
    testlistMult,
    testlistRecip
    ]


testcaseMult x y z = TestCase $ assertEqual (show x ++ "*" ++ show y ++ "==" ++ show z) z (x*y)

testlistMult = TestList [
    testcaseMult (sqrt 2) (sqrt 2) 2,
    testcaseMult (sqrt 2) (sqrt 3) (sqrt 6),
    testcaseMult (sqrt 2) (sqrt 6) (2 * sqrt 3),
    testcaseMult i i (-1),
    testcaseMult i (i * sqrt 3) (-1 * sqrt 3),
    testcaseMult (i * sqrt 2) (i * sqrt 3) (-1 * sqrt 6)
    ]
-- We don't bother to test multiplication of sums, because it's obvious by definition that it will work


testcaseRecip x = TestCase $ assertBool ("recip " ++ show x) (x * recip x == 1)

testlistRecip = TestList [
    testcaseRecip (sqrt 2),
    testcaseRecip i,
    testcaseRecip (sqrt 2 + sqrt 3),
    testcaseRecip (sqrt 2 + 2 * sqrt 3),
    testcaseRecip (sqrt 2 + sqrt 6),
    testcaseRecip (sqrt 2 + sqrt 3 + sqrt 5),
    testcaseRecip (i + 3*sqrt 2 + 2*sqrt 3 - sqrt 5 + 5*sqrt 11)
    ]
-- These tests could be replaced with QuickCheck equivalents, provided we limited the Arbitrary instance
-- to avoid having to solve too large a linear system