-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}

-- |A module defining the quantum plane and its symmetries
module Math.QuantumAlgebra.QuantumPlane where

-- Refs:
-- Kassel, Quantum Groups
-- Street, Quantum Groups

import Math.Algebra.Field.Base hiding (powers)

import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures
import Math.Algebras.LaurentPoly
import Math.Algebras.NonCommutative
import qualified Data.List as L



qvar v = let V [(m,1)] = var v in V [(m,1 :: LaurentPoly Q)]


a = qvar "a"
b = qvar "b"
c = qvar "c"
d = qvar "d"

detq = a*d-unit q'*b*c


x = qvar "x"
y = qvar "y"
-- z = qvar "z"

u = qvar "u"
v = qvar "v"


-- Quantum plane Aq20

aq20 = [y*x-unit q*x*y]
-- Kassel p72, Street p10

newtype Aq20 v = Aq20 (NonComMonomial v) deriving (Eq,Ord)

instance (Eq v, Show v) => Show (Aq20 v) where show (Aq20 m) = show m

instance Monomial Aq20 where
    var v = V [(Aq20 (NCM 1 [v]),1)]
    powers (Aq20 m) = powers m

instance Algebra (LaurentPoly Q) (Aq20 String) where
    unit 0 = zero -- V []
    unit x = V [(munit,x)] where munit = Aq20 (NCM 0 [])
    mult x = x''' where
        x' = mult $ fmap ( \(Aq20 a, Aq20 b) -> (a,b) ) x -- unwrap and multiply
        x'' = x' %% aq20 -- quotient by m2q relations while unwrapped
        x''' = fmap Aq20 x'' -- wrap the monomials up as Aq20 again


-- Quantum superplane Aq02

aq02 = [u^2, v^2, u*v+unit q*v*u]
-- Street p10

newtype Aq02 v = Aq02 (NonComMonomial v) deriving (Eq,Ord)

instance (Eq v, Show v) => Show (Aq02 v) where show (Aq02 m) = show m

instance Monomial Aq02 where
    var v = V [(Aq02 (NCM 1 [v]),1)]
    powers (Aq02 m) = powers m

instance Algebra (LaurentPoly Q) (Aq02 String) where
    unit 0 = zero -- V []
    unit x = V [(munit,x)] where munit = Aq02 (NCM 0 [])
    mult x = x''' where
        x' = mult $ fmap ( \(Aq02 a, Aq02 b) -> (a,b) ) x -- unwrap and multiply
        x'' = x' %% aq02 -- quotient by m2q relations while unwrapped
        x''' = fmap Aq02 x'' -- wrap the monomials up as Aq02 again


-- M2q

m2q = [a*b-unit q'*b*a, a*c-unit q'*c*a, c*d-unit q'*d*c, b*d-unit q'*d*b,
       b*c-c*b, a*d-d*a-unit (q'-q)*b*c]
-- Kassel p78, Street p9
-- I think this is already a Groebner basis

newtype M2q v = M2q (NonComMonomial v) deriving (Eq,Ord)

instance (Eq v, Show v) => Show (M2q v) where show (M2q m) = show m

instance Monomial M2q where
    var v = V [(M2q (NCM 1 [v]),1)]
    powers (M2q m) = powers m

instance Algebra (LaurentPoly Q) (M2q String) where
    unit 0 = zero -- V []
    unit x = V [(munit,x)] where munit = M2q (NCM 0 [])
    mult x = x''' where
        x' = mult $ fmap ( \(M2q a, M2q b) -> (a,b) ) x -- unwrap and multiply
        x'' = x' %% m2q -- quotient by m2q relations while unwrapped
        x''' = fmap M2q x'' -- wrap the monomials up as M2q again

-- Kassel p82-3
instance Coalgebra (LaurentPoly Q) (M2q String) where
    counit x = case x `bind` cu of
               V [] -> 0
               V [(M2q (NCM 0 []), c)] -> c
        where cu "a" = 1 :: Vect (LaurentPoly Q) (M2q String)
              cu "b" = 0
              cu "c" = 0
              cu "d" = 1
    comult x = x `bind` cm
        where cm "a" = a `te` a + b `te` c
              cm "b" = a `te` b + b `te` d
              cm "c" = c `te` a + d `te` c
              cm "d" = c `te` b + d `te` d

instance Bialgebra (LaurentPoly Q) (M2q String) where {}

{-
-- The following shows that the M2q relations are *sufficient*
-- for M2q to be symmetries of Aq20 and Aq02

> let x' = a*x+b*y :: Vect (LaurentPoly Q) (NonComMonomial String)
> let y' = c*x+d*y :: Vect (LaurentPoly Q) (NonComMonomial String)
> (y'*x'-unit q*x'*y') %% (m2q ++ aq20 ++ [s*t-t*s | s <- [a,b,c,d], t <- [x,y]])
0

> let u' = a*u+b*v :: Vect (LaurentPoly Q) (NonComMonomial String)
> let v' = c*u+d*v :: Vect (LaurentPoly Q) (NonComMonomial String)
> (u'^2) %% (m2q ++ aq02 ++ [s*t-t*s | s <- [a,b,c,d], t <- [u,v]])
0
> (v'^2) %% (m2q ++ aq02 ++ [s*t-t*s | s <- [a,b,c,d], t <- [u,v]])
0
> (u'*v'+unit q*v'*u') %% (m2q ++ aq02 ++ [s*t-t*s | s <- [a,b,c,d], t <- [u,v]])
0

-- To show that the M2q relations are necessary,
-- set the coefficients of x^2, yx, y^2, and vu == 0 in all of the following
> (y'*x'-unit q*x'*y') %% (aq20 ++ [p*q-q*p | p <- [a,b,c,d], q <- [x,y]])
-qx^2ac+x^2ca-yxad-qyxbc+q^-1yxcb+yxda-qy^2bd+y^2db
> (u'^2) %% (aq02 ++ [p*q-q*p | p <- [a,b,c,d], q <- [u,v]])
-qvuab+vuba
> (v'^2) %% (aq02 ++ [p*q-q*p | p <- [a,b,c,d], q <- [u,v]])
-qvucd+vudc
> (u'*v'+unit q*v'*u') %% (aq02 ++ [p*q-q*p | p <- [a,b,c,d], q <- [u,v]])
-qvuad+vubc-q^2vucb+qvuda

-- yx => -ad-qbc+q^-1cb+da == 0
-- vu => -qad+bc-q^2cb+qda == 0
-- qyx-vu => -q^2bc+cb-bc+q^2cb == 0 => bc == cb
-- Now substitute back into yx

-- We could probably have got gb to do this for us
-}

-- Kassel p85
instance Comodule (LaurentPoly Q) (M2q String) (Aq20 String) where
    coaction xy = xy `bind` ca where
        ca "x" = (a `te` x) + (b `te` y) -- we can use (+) instead of add since Aq20 is an algebra 
        ca "y" = (c `te` x) + (d `te` y)
-- coaction (x) = (a b) `te` (x)
--          (y)   (c d)      (y)


-- SL2q

sl2q = [a*b-unit q'*b*a, a*c-unit q'*c*a, c*d-unit q'*d*c, b*d-unit q'*d*b,
        b*c-c*b, a*d-d*a-unit (q'-q)*b*c,
        -unit q*c*b + d*a - 1] -- det q, but reduced
--        a*d-unit q'*b*c-1] -- det_q
-- We have to hand-reduce detq, or else call gb



newtype SL2q v = SL2q (NonComMonomial v) deriving (Eq,Ord)

instance (Eq v, Show v) => Show (SL2q v) where show (SL2q m) = show m

instance Monomial SL2q where
    var v = V [(SL2q (NCM 1 [v]),1)]
    powers (SL2q m) = powers m

instance Algebra (LaurentPoly Q) (SL2q String) where
    unit 0 = zero -- V []
    unit x = V [(munit,x)] where munit = SL2q (NCM 0 [])
    mult x = x''' where
        x' = mult $ fmap ( \(SL2q a, SL2q b) -> (a,b) ) x -- unwrap and multiply
        x'' = x' %% sl2q -- quotient by sl2q relations while unwrapped
        x''' = fmap SL2q x'' -- wrap the monomials up as SL2q again

instance Coalgebra (LaurentPoly Q) (SL2q String) where
    counit x = case x `bind` cu of
               V [] -> 0
               V [(SL2q (NCM 0 []), c)] -> c
        where cu "a" = 1 :: Vect (LaurentPoly Q) (SL2q String)
              cu "b" = 0
              cu "c" = 0
              cu "d" = 1
    comult x = x `bind` cm
        where cm "a" = a `te` a + b `te` c
              cm "b" = a `te` b + b `te` d
              cm "c" = c `te` a + d `te` c
              cm "d" = c `te` b + d `te` d

instance Bialgebra (LaurentPoly Q) (SL2q String) where {}

-- Kassel p84
instance HopfAlgebra (LaurentPoly Q) (SL2q String) where
    antipode x = x `bind` antipode'
        where antipode' "a" = d
              antipode' "b" = - unit q * b
              antipode' "c" = - unit q' * c
              antipode' "d" = a
-- in the GL2q case we would need 1/detq factor as well



-- !! The following probably needs to be rehoused in separate module at some point
-- YANG-BAXTER OPERATOR

-- This is a Yang-Baxter operator, but not the only possible such
-- Street, p93
yb x = nf $ x >>= yb' where
    yb' (a,b) = case compare a b of
                 GT -> return (b,a)
                 LT -> return (b,a) + unit (q-q') * return (a,b)
                 EQ -> unit q * return (a,a)


