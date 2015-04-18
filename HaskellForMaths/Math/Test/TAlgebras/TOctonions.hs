-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TAlgebras.TOctonions where

import Test.QuickCheck

import Math.Core.Field
-- import Math.Algebra.Field.Base
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Quaternions
import Math.Algebras.Octonions

import Math.Test.TAlgebras.TVectorSpace
import Math.Test.TAlgebras.TStructures

import Math.Algebras.Structures -- not really needed

instance Arbitrary OBasis where
    arbitrary = elements $ map O [-1..6]

-- TVectorSpace defines an Arbitrary instance for Vect k b, given Arbitrary instances for k and b

-- same as prop_Algebra, but missing associativity axiom
prop_AlgebraNonAssociative (k,x) =
    unitOutL (k' `te` x) == (mult . (unit' `tf` id)) (k' `te` x)  && -- left unit
    unitOutR (x `te` k') == (mult . (id `tf` unit')) (x `te` k')     -- right unit
    where k' = k *> return ()

prop_AlgebraNonAssociative_Octonions (k,x) = prop_AlgebraNonAssociative (k,x)
    where types = (k,x) :: (Q, Octonion Q)

prop_InverseLoop (x,y) =
    x*1 == x && x == 1*x &&
    (x == 0 ||
      (x^-1 * (x*y) == y && y == (y*x) * x^-1 &&
       (x^-1)^-1 == x) )

prop_InverseLoop_Octonions (x,y) = prop_InverseLoop (x,y)
    where types = (x,y) :: (Octonion Q, Octonion Q)
