-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE EmptyDataDecls, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, RankNTypes #-}

module Math.Test.TAlgebras.TTensorProduct where

import Test.QuickCheck
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Field
-- import Math.Algebra.Field.Base
import Math.Test.TAlgebras.TVectorSpace

import Prelude as P
import Control.Category as C
import Control.Arrow


quickCheckTensorProduct = do
    putStrLn "Testing that tf is linear, and tensor product is a functor"
    quickCheck prop_Linear_tf
    quickCheck prop_TensorFunctor


type DirectSum k u v =
    (u ~ Vect k a, v ~ Vect k b) => Vect k (DSum a b)

type TensorProd k u v =
    (u ~ Vect k a, v ~ Vect k b) => Vect k (Tensor a b)

type En = Vect Q EBasis

{-
-- But then you need to make sure that you run GHCi with -XTypeFamilies, otherwise:

> e1 `te` e2 :: TensorProd Q En En
<interactive>:1:0:
    Illegal equational constraint En ~ Vect Q a
    (Use -XTypeFamilies to permit this)
    In an expression type signature: TensorProd Q En En
    In the expression: e1 `te` e2 :: TensorProd Q En En
    In the definition of `it': it = e1 `te` e2 :: TensorProd Q En En
-}

-- check that tf is linear
prop_Linear_tf ((f,g),k,(a1,a2,b1,b2)) = prop_Linear (linfun f `tf` linfun g) (k, a1 `te` b1, a2 `te` b2)
    where types = (f,g,k,a1,a2,b1,b2) :: (LinFun Q ABasis SBasis, LinFun Q BBasis TBasis, Q,
                                          Vect Q ABasis, Vect Q ABasis, Vect Q BBasis, Vect Q BBasis)

 
-- check that tensor product is a functor, as required
prop_TensorFunctor ((f1,f2,g1,g2),(a,b)) =
    (P.id `tf` P.id) (a `te` b) == P.id (a `te` b) &&
    ((f' P.. f) `tf` (g' P.. g)) (a `te` b) == ((f' `tf` g') P.. (f `tf` g)) (a `te` b)
    where f = linfun f1
          f' = linfun f2
          g = linfun g1
          g' = linfun g2
          types = (f1,f2,g1,g2,a,b) :: (LinFun Q ABasis ABasis, LinFun Q ABasis ABasis,
                                        LinFun Q BBasis BBasis, LinFun Q BBasis BBasis,
                                        Vect Q ABasis, Vect Q BBasis)


-- Now test eg
-- > quickCheck (\x -> (distrL . undistrL) x == id x)
-- but need to make x be of interesting type (not just () )


data Zero
-- a type with no inhabitants
-- so the associated free vector space is the zero space

-- instance Eq Zero where {}
-- instance Ord Zero where {}
instance Show Zero where {}

-- > zero :: Vect Q Zero
-- 0


-- ARROW INSTANCE
-- This isn't currently used anywhere else
-- It's intended to illustrate the point that tensor product is like doing things in parallel

newtype Linear k a b = Linear (Vect k a -> Vect k b)

instance Category (Linear k) where
    id = Linear P.id
    (Linear f) . (Linear g) = Linear (f P.. g)

instance (Eq k, Num k) => Arrow (Linear k) where
    arr f = Linear (fmap f) -- requires nf call afterwards
    first (Linear f) = Linear f *** Linear P.id
    second (Linear f) = Linear P.id *** Linear f
    Linear f *** Linear g = Linear (f `tf2` g)
        where tf2 f g (V ts) = V $ concat
                  [let V us = x *> te (f $ return a) (g $ return b) in us | ((a,b), x) <- ts]
        -- can't use tf, as it uses add, which assumes Ord instance
    Linear f &&& Linear g = (Linear f *** Linear g) C.. Linear (\a -> a `te` a)

{-
-- The following are morally correct, but don't work because they require Ord instance
instance Num k => ArrowChoice (Linear k) where
    left (Linear f) = Linear (f `dsume` id)
    right (Linear f) = Linear (id `dsume` f)
    Linear f +++ Linear g = Linear (f `dsumf` g)
    Linear f ||| Linear g = Linear (f `coprodf` g)
-}

