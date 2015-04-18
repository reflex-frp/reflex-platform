-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Math.Test.TAlgebras.TGroupAlgebra where

import Test.HUnit
import Test.QuickCheck

import Math.Algebra.Group.PermutationGroup hiding (p)
import Math.Test.TPermutationGroup -- for instance Arbitrary (Permutation Int)

import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures
import Math.Algebras.GroupAlgebra
import Math.Core.Field
import Math.Core.Utils

import Math.Test.TAlgebras.TVectorSpace -- for instance Arbitrary Q and (Vect k b)
import Math.Test.TAlgebras.TStructures -- for quickcheck properties

prop_Algebra_GroupAlgebra (k,x,y,z) = prop_Algebra (k,x,y,z)
    where types = (k,x,y,z) :: (Q, GroupAlgebra Q, GroupAlgebra Q, GroupAlgebra Q)

-- have to split the 8-tuple into two 4-tuples to avoid having to write Arbitrary instance
prop_Algebra_Linear_GroupAlgebra ((k,l,m,n),(x,y,z,w)) = prop_Algebra_Linear (k,l,m,n,x,y,z,w)
    where types = (k,l,m,n,x,y,z,w) :: (Q, Q, Q, Q,
                   GroupAlgebra Q, GroupAlgebra Q, GroupAlgebra Q, GroupAlgebra Q)

prop_Coalgebra_GroupAlgebra x = prop_Coalgebra x
    where types = x :: GroupAlgebra Q

prop_Coalgebra_Linear_GroupAlgebra (k,l,x,y) = prop_Coalgebra_Linear (k,l,x,y)
    where types = (k,l,x,y) :: (Q, Q, GroupAlgebra Q, GroupAlgebra Q)

prop_Bialgebra_GroupAlgebra (k,x,y) = prop_Bialgebra (k,x,y)
    where types = (k,x,y) :: (Q, GroupAlgebra Q, GroupAlgebra Q)

prop_HopfAlgebra_GroupAlgebra x = prop_HopfAlgebra x
    where types = x :: GroupAlgebra Q


quickCheckGroupAlgebra = do
    putStrLn "Testing that group algebra is an algebra, coalgebra, bialgebra, and Hopf algebra..."
    quickCheck prop_Algebra_GroupAlgebra
    quickCheck prop_Coalgebra_GroupAlgebra
    quickCheck prop_Bialgebra_GroupAlgebra
    quickCheck prop_HopfAlgebra_GroupAlgebra
    quickCheck (prop_AlgebraAntiMorphism (antipode :: GroupAlgebra Q -> GroupAlgebra Q))
    quickCheck (prop_CoalgebraAntiMorphism (antipode :: GroupAlgebra Q -> GroupAlgebra Q))


testlistGroupAlgebra = TestList [
    testlistLeftInverse,
    testlistRightInverse
    ]

groupAlgebraElts = [ 1+p[[1,2,3]], 1+p[[1,2,3]]+p[[1,2],[3,4]], 1+2*p[[1,2,3]]+p[[1,2],[3,4]] ]

testcaseLeftInverse x = TestCase $ assertEqual ("inverse " ++ show x) 1 (x^-1 * x)

testlistLeftInverse = TestList $ map testcaseLeftInverse groupAlgebraElts 

testcaseRightInverse x = TestCase $ assertEqual ("inverse " ++ show x) 1 (x * x^-1)

testlistRightInverse = TestList $ map testcaseRightInverse groupAlgebraElts 
