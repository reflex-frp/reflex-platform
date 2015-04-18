-- Copyright (c) David Amos, 2009-2011. All rights reserved.

module Math.Test.TProjects.TMiniquaternionGeometry where

{-
import qualified Data.List as L

import Math.Common.ListSet as LS

import Math.Algebra.Field.Base
import Math.Combinatorics.FiniteGeometry (pnf, ispnf, orderPGL)
import Math.Combinatorics.Graph (combinationsOf)
import Math.Combinatorics.GraphAuts
import Math.Algebra.Group.PermutationGroup hiding (order)
import qualified Math.Algebra.Group.SchreierSims as SS
import Math.Algebra.Group.RandomSchreierSims
import Math.Combinatorics.Design as D
import Math.Algebra.LinearAlgebra -- ( (<.>), (<+>) )

import Math.Projects.ChevalleyGroup.Classical
-}

import Test.QuickCheck
import Math.Projects.MiniquaternionGeometry


-- Near fields

prop_NearField (a,b,c) =
    a+(b+c) == (a+b)+c   &&  -- addition is associative
    a+b == b+a           &&  -- addition is commutative
    a+0 == a             &&  -- additive identity
    a+(-a) == 0          &&  -- additive inverse
    a*(b*c) == (a*b)*c   &&  -- multiplication is associative
    a*1 == a && 1*a == a &&  -- multiplicative identity
    (a+b)*c == a*c + b*c &&  -- right-distributivity
    a*0 == 0

instance Arbitrary F9 where
    arbitrary = do x <- arbitrary :: Gen Int
                   return (f9 !! (x `mod` 9))

instance Arbitrary J9 where
    arbitrary = do x <- arbitrary :: Gen Int
                   return (j9 !! (x `mod` 9))

prop_NearFieldF9 (a,b,c) = prop_NearField (a,b,c) where
    types = (a,b,c) :: (F9,F9,F9)

prop_NearFieldJ9 (a,b,c) = prop_NearField (a,b,c) where
    types = (a,b,c) :: (J9,J9,J9)

