-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Test.TAlgebras.TQuaternions where

import Test.QuickCheck

import Math.Algebra.Field.Base
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Quaternions

import Math.Test.TAlgebras.TVectorSpace
import Math.Test.TAlgebras.TStructures

import Math.Algebras.Structures -- not really needed

instance Arbitrary HBasis where
    arbitrary = elements [One,I,J,K]

-- TVectorSpace defines an Arbitrary instance for Vect k b, given Arbitrary instances for k and b
{-
instance Arbitrary (Quaternion Integer) where
    arbitrary = do ts <- arbitrary :: Gen [(HBasis, Integer)]
                   return $ nf $ V ts
-}


prop_Algebra_Quaternion (k,x,y,z) = prop_Algebra (k,x,y,z)
    where types = (k,x,y,z) :: (Q, Quaternion Q, Quaternion Q, Quaternion Q)
-- (Integer, Quaternion Integer, Quaternion Integer, Quaternion Integer)

prop_Coalgebra_DualQuaternion x = prop_Coalgebra x
    where types = x :: Vect Q (Dual HBasis)

conjH = linear conjH'
    where conjH' One =  1
          conjH' I   = -i
          conjH' J   = -j
          conjH' K   = -k

normH x = x * conjH x


-- The following property fails: conjugation is not an algebra morphism
-- It fails to commute with mult: conjH (i*j) /= conjH i * conjH j
nonprop_AlgebraMorphism_ConjH = prop_AlgebraMorphism conjH

-- The following property also fails: norm is not an algebra morphism
-- It fails to commute with unit: conjH (unit (-1)) /= unit (-1)
nonprop_AlgebraMorphism_NormH = prop_AlgebraMorphism normH


{-
prop_Coalgebra_Quaternion x = prop_Coalgebra x
    where types = x :: Quaternion Integer

-- Fails - the algebra and coalgebra structures I've given are not compatible
prop_Bialgebra_Quaternion (k,x,y) = prop_Bialgebra (k,x,y)
    where types = (k,x,y) :: (Integer, Quaternion Integer, Quaternion Integer)
-}
{-
prop_FrobeniusRelation_Quaternion (x,y) = prop_FrobeniusRelation (x,y)
    where types = (x,y) :: (Quaternion Integer, Quaternion Integer)
-- !! fails, because the counit we have given is not a Frobenius form
-}

instance Algebra2 Integer (Quaternion Integer) where
    unit2 k = unit k
    mult2 xy = mult xy
