-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |A module defining the affine plane and its symmetries
module Math.Algebras.AffinePlane where

import Math.Algebra.Field.Base hiding (powers)
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures
import Math.Algebras.Commutative


data XY = X | Y deriving (Eq, Ord)

instance Show XY where show X = "x"; show Y = "y"

x = glexVar X :: GlexPoly Q XY
y = glexVar Y :: GlexPoly Q XY


data ABCD = A | B | C | D deriving (Eq, Ord)

instance Show ABCD where show A = "a"; show B = "b"; show C = "c"; show D = "d"

a,b,c,d :: Monomial m => Vect Q (m ABCD)
a = var A
b = var B
c = var C
d = var D


-- SL2

newtype SL2 v = SL2 (GlexMonomial v) deriving (Eq,Ord)

instance Show v => Show (SL2 v) where show (SL2 m) = show m

instance Algebra Q (SL2 ABCD) where -- to do this for Num k instead of Q we would need a,b,c,d defined for Num k
    unit 0 = zero -- V []
    unit x = V [(munit,x)] where munit = SL2 (Glex 0 [])
    mult x = x''' where
        x' = mult $ fmap ( \(SL2 a, SL2 b) -> (a,b) ) x -- perform the multiplication in GlexPoly
        x'' = x' %% [a*d-b*c-1] -- :: GlexPoly Q ABCD] -- quotient by ad-bc=1 in GlexPoly Q ABCD
        x''' = fmap SL2 x'' -- ie wrap the monomials up as SL2 again
        -- mmult (Glex si xis) (Glex sj yjs) = Glex (si+sj) $ addmerge xis yjs

sl2Var v = V [(SL2 (Glex 1 [(v,1)]), 1)] -- :: Vect Q (SL2 ABCD)


-- For example:
-- > a*d :: Vect Q (SL2 ABCD)
-- bc+1

instance Monomial SL2 where
    var = sl2Var
    powers (SL2 (Glex _ xis)) = xis



instance Coalgebra Q (SL2 ABCD) where
    counit x = case x `bind` cu of
               V [] -> 0
               V [(SL2 (Glex 0 []), c)] -> c
        where cu A = 1 :: Vect Q (SL2 ABCD)
              cu B = 0
              cu C = 0
              cu D = 1
    comult x = x `bind` cm
        where cm A = a `te` a + b `te` c
              cm B = a `te` b + b `te` d
              cm C = c `te` a + d `te` c
              cm D = c `te` b + d `te` d
-- In other words
-- counit (a b) = (1 0)
--        (c d)   (0 1)
-- comult (a b) = (a1 b1) `te` (a2 b2)
--        (c d)   (c1 d1)      (c2 d2)

instance Bialgebra Q (SL2 ABCD) where {}

instance HopfAlgebra Q (SL2 ABCD) where
    antipode x = x `bind` antipode'
        where antipode' A = d
              antipode' B = b
              antipode' C = c
              antipode' D = a
-- in the GL2 case we would need 1/det factor as well

