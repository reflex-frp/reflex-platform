-- Copyright (c) David Amos, 2008. All rights reserved.

{-# LANGUAGE FlexibleInstances #-}

module Math.Test.TNonCommutativeAlgebra where

import Math.Algebra.Field.Base
import Math.Algebra.NonCommutative.NCPoly
import Math.Algebra.NonCommutative.TensorAlgebra

import Test.QuickCheck

-- > quickCheck prop_NonCommRingNPoly

-- Non-Commutative Ring (with 1)
prop_NonCommRing (a,b,c) =
    a+(b+c) == (a+b)+c   &&  -- addition is associative
    a+b == b+a           &&  -- addition is commutative
    a+0 == a             &&  -- additive identity
    a+(-a) == 0          &&  -- additive inverse
    a*(b*c) == (a*b)*c   &&  -- multiplication is associative
    a*1 == a && 1*a == a &&  -- multiplicative identity
    a*(b+c) == a*b + a*c &&  -- left distributivity
    (a+b)*c == a*c + b*c     -- left distributivity

monomial is = product $ map (e_ . abs) is

-- npoly :: [(Integer,[Int])] -> NPoly Q Basis
npoly ais = sum [fromInteger a * monomial is | (a,is) <- ais]

instance Arbitrary (NPoly Q Basis) where
    -- arbitrary = do ais <- arbitrary :: Gen [(Integer,[Int])]
    arbitrary = do ais <- sized $ \n -> resize (n `div` 2) arbitrary :: Gen [(Integer,[Int])]
                   return (npoly ais)
    -- coarbitrary = undefined -- !! only required if we want to test functions over the type

prop_NonCommRingNPoly (f,g,h) = prop_NonCommRing (f,g,h) where
    types = (f,g,h) :: (NPoly Q Basis, NPoly Q Basis, NPoly Q Basis)

