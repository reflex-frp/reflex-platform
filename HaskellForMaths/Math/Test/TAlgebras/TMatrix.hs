-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Test.TAlgebras.TMatrix where

import Test.QuickCheck

import Math.Algebra.Field.Base

import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Matrix

import Math.Test.TAlgebras.TVectorSpace
import Math.Test.TAlgebras.TStructures

import Math.Algebras.Structures -- not really needed

instance Arbitrary Mat2 where
    arbitrary = elements [E2 1 1, E2 1 2, E2 2 1, E2 2 2]

instance Arbitrary Mat2' where
    arbitrary = elements [E2' 1 1, E2' 1 2, E2' 2 1, E2' 2 2]


prop_Algebra_Mat2 (k,x,y,z) = prop_Algebra (k,x,y,z)
    where types = (k,x,y,z) :: (Q, Vect Q Mat2, Vect Q Mat2, Vect Q Mat2)

prop_Coalgebra_Mat2' x = prop_Coalgebra x
    where types = x :: Vect Q Mat2'
