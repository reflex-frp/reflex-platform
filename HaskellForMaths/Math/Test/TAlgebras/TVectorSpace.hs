-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}


module Math.Test.TAlgebras.TVectorSpace where

import Test.QuickCheck
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Field
-- import Math.Algebra.Field.Base

-- import Control.Monad -- MonadPlus


prop_AddGrp (x,y,z) =
    x <+> (y <+> z) == (x <+> y) <+> z && -- associativity
    x <+> y == y <+> x                 && -- commutativity
    x <+> zero == x                    && -- identity
    x <+> neg x == zero                   -- inverse

prop_VecSp (a,b,x,y,z) =
    prop_AddGrp (x,y,z) &&
    a *> (x <+> y) == a *> x <+> a *> y && -- distributivity through vectors
    (a+b) *> x == a *> x <+> b *> x     && -- distributivity through scalars
    (a*b) *> x == a *> (b *> x)         && -- associativity
    1 *> x == x                            -- unit

instance Arbitrary EBasis where
    arbitrary = do n <- arbitrary :: Gen Int
                   return (E $ abs n)

instance Arbitrary b => Arbitrary (Dual b) where
    arbitrary = fmap Dual arbitrary
--     arbitrary = do b <- arbitrary :: Gen b -- ScopedTypeVariables
--                    return (Dual b)

instance Arbitrary Q where
    arbitrary = do n <- arbitrary :: Gen Integer
                   d <- arbitrary :: Gen Integer
                   return (if d == 0 then fromInteger n else fromInteger n / fromInteger d)

instance (Eq k, Num k, Ord b, Arbitrary k, Arbitrary b) => Arbitrary (Vect k b) where
    arbitrary = do ts <- arbitrary :: Gen [(b, k)] -- ScopedTypeVariables
                   return $ nf $ V $ take 3 ts
-- we impose a complexity bound of 3 terms to limit to 27 terms when testing associativity and ensure reasonable running time

prop_VecSpQn (a,b,x,y,z) = prop_VecSp (a,b,x,y,z)
    where types = (a,b,x,y,z) :: (Q, Q, Vect Q EBasis, Vect Q EBasis, Vect Q EBasis)


prop_Linear f (a,x,y) =
    f (x <+> y) == f x <+> f y &&
    f zero == zero             &&
    f (neg x) == neg (f x)     &&
    f (a *> x) == a *> f x

prop_LinearQn f (a,x,y) = prop_Linear f (a,x,y)
    where types = (a,x,y) :: (Q, Vect Q EBasis, Vect Q EBasis)


newtype FBasis = F Int deriving (Eq,Ord,Arbitrary)

instance Show FBasis where show (F i) = "f" ++ show i

f i = return (F i) :: Vect Q FBasis
f1 = f 1
f2 = f 2
f3 = f 3


-- DIRECT SUM

{-
instance Num k => MonadPlus (Vect k) where
    mzero = zero
    mplus (V xs) (V ys) = V (xs++ys) -- need to call nf afterwards
-}


-- (Alternative versions of prodf and coprodf)

f .*. g = linear fg' where
    fg' b = fmap Left (f (return b)) <+> fmap Right (g (return b))

f .+. g = linear fg' where
    fg' (Left a) = f (return a)
    fg' (Right b) = g (return b)


type LinFun k a b = [(a, Vect k b)]
-- a way of representing a linear function as data

linfun :: (Eq k, Num k, Eq a, Ord b) => LinFun k a b -> Vect k a -> Vect k b
linfun avbs = linear f where
    f a = case lookup a avbs of
          Just vb -> vb
          Nothing -> zero


prop_Product (f',g',x) =
    f x == (p1 . fg) x &&
    g x == (p2 . fg) x
    where f = linfun f'
          g = linfun g'
          fg = prodf f g

prop_Coproduct (f',g',a,b) =
    f a == (fg . i1) a &&
    g b == (fg . i2) b
    where f = linfun f'
          g = linfun g'
          fg = coprodf f g

prop_dsumf (f',g',a,b) =
    f a == (p1 . fg . i1) a &&
    g b == (p2 . fg . i2) b
    where f = linfun f'
          g = linfun g'
          fg = dsumf f g


newtype ABasis = A Int deriving (Eq,Ord,Show,Arbitrary) -- GeneralizedNewtypeDeriving
newtype BBasis = B Int deriving (Eq,Ord,Show,Arbitrary)
newtype SBasis = S Int deriving (Eq,Ord,Show,Arbitrary)
newtype TBasis = T Int deriving (Eq,Ord,Show,Arbitrary)

prop_ProductQn (f,g,x) = prop_Product (f,g,x)
    where types = (f,g,x) :: (LinFun Q SBasis ABasis, LinFun Q SBasis BBasis, Vect Q SBasis)

prop_CoproductQn (f,g,a,b) = prop_Coproduct (f,g,a,b)
    where types = (f,g,a,b) :: (LinFun Q ABasis TBasis, LinFun Q BBasis TBasis, Vect Q ABasis, Vect Q BBasis)

prop_dsumfQn (f,g,a,b) = prop_dsumf (f,g,a,b)
    where types = (f,g,a,b) :: (LinFun Q ABasis SBasis, LinFun Q BBasis TBasis, Vect Q ABasis, Vect Q BBasis)


-- TENSOR PRODUCT

dot0 uv = sum [ if a == b then x*y else 0 | (a,x) <- u, (b,y) <- v]
    where V u = p1 uv
          V v = p2 uv

dot1 uv = nf $ V [( (), if a == b then x*y else 0) | (a,x) <- u, (b,y) <- v]
    where V u = p1 uv
          V v = p2 uv

polymult1 uv = nf $ V [(E (i+j) , x*y) | (E i,x) <- u, (E j,y) <- v]
    where V u = p1 uv
          V v = p2 uv

{-
tensor1 :: (Num k, Ord a, Ord b) => (Vect k a, Vect k b) -> Vect k (a, b)
tensor1 (V axs, V bys) = nf $ V [((a,b),x*y) | (a,x) <- axs, (b,y) <- bys] 

bilinear1 :: (Num k, Ord a, Ord b, Ord c) =>
     ((a, b) -> Vect k c) -> (Vect k a, Vect k b) -> Vect k c
bilinear1 f = linear f . tensor1

prop_Bilinear1 f (a,u1,u2,v1,v2) =
    prop_Linear (\v -> f (u1,v)) (a,v1,v2) &&
    prop_Linear (\u -> f (u,v1)) (a,u1,u2)

prop_BilinearQn1 f (a,u1,u2,v1,v2) = prop_Bilinear1 f (a,u1,u2,v1,v2)
    where types = (a,u1,u2,v1,v2) :: (Q, Vect Q EBasis, Vect Q EBasis, Vect Q EBasis, Vect Q EBasis)
-}

tensor :: (Eq k, Num k, Ord a, Ord b) => Vect k (Either a b) -> Vect k (a, b)
tensor uv = nf $ V [( (a,b), x*y) | (a,x) <- u, (b,y) <- v]
    where V u = p1 uv; V v = p2 uv

bilinear :: (Eq k, Num k, Ord a, Ord b, Ord c) =>
    ((a, b) -> Vect k c) -> Vect k (Either a b) -> Vect k c
bilinear f = linear f . tensor

dot = bilinear (\(a,b) -> if a == b then return () else zero)

polymult = bilinear (\(E i, E j) -> return (E (i+j)))

prop_Bilinear :: (Eq k, Num k, Ord a, Ord b, Ord t) =>
     (Vect k (Either a b) -> Vect k t) -> (k, Vect k a, Vect k a, Vect k b, Vect k b) -> Bool
prop_Bilinear f (a,u1,u2,v1,v2) =
    prop_Linear (\v -> f (u1 `dsume` v)) (a,v1,v2) &&
    prop_Linear (\u -> f (u `dsume` v1)) (a,u1,u2)

prop_BilinearQn f (a,u1,u2,v1,v2) = prop_Bilinear f (a,u1,u2,v1,v2)
    where types = (a,u1,u2,v1,v2) :: (Q, Vect Q EBasis, Vect Q EBasis, Vect Q EBasis, Vect Q EBasis)

{-
> quickCheck (prop_BilinearQn dot1)
+++ OK, passed 100 tests.
> quickCheck (prop_BilinearQn polymult1)
+++ OK, passed 100 tests.
*Math.Test.TAlgebras.TVectorSpace> quickCheck (prop_BilinearQn tensor)
+++ OK, passed 100 tests.

> quickCheck (\x -> dot1 x == dot x)
+++ OK, passed 100 tests.
> quickCheck (\x -> polymult1 x == polymult x)
+++ OK, passed 100 tests.


> quickCheck (prop_BilinearQn id)
*** Failed! Falsifiable (after 2 tests):  
(1,0,0,e1,0)
-- fails basically because (0 <+> 0) `dsume` e0 /= (0 `dsume` e0) <+> (0 `dsume` e0)

>  (zero <+> zero) `dsume` e1
Right e1
> (zero `dsume` e1) <+> (zero `dsume` e1)
2Right e1

-}

