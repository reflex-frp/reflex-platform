-- Copyright (c) David Amos, 2008. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, ScopedTypeVariables, EmptyDataDecls, FlexibleInstances #-}

module Math.Algebra.Field.Extension where

import Data.Ratio
import Data.List as L (elemIndex)

import Math.Common.IntegerAsType
import Math.Core.Utils
import Math.Algebra.Field.Base


-- UNIVARIATE POLYNOMIALS

newtype UPoly a = UP [a] deriving (Eq,Ord)
-- the list [a_0, a_1, ..., a_n] represents the polynomial a_0 + a_1 x + ... + a_n x^n

x = UP [0,1] :: UPoly Integer

instance (Eq a, Show a, Num a) => Show (UPoly a) where
    -- show (UP []) = "0"
    show (UP as) = showUP "x" as

showUP _ [] = "0"
showUP v as = let powers = filter ( (/=0) . fst ) $ zip as [0..]
                  c:cs = concatMap showTerm powers
              in if c == '+' then cs else c:cs
    where showTerm (a,i) = showCoeff a ++ showPower a i
          showCoeff a = case show a of
                        "1" -> "+"
                        "-1" -> "-"
                        '-':cs -> '-':cs
                        cs -> '+':cs
          showPower a i | i == 0 = case show a of
                                   "1" -> "1"
                                   "-1" -> "1"
                                   otherwise -> ""
                        | i == 1 = v -- "x"
                        | i > 1  = v ++ "^" ++ show i -- "x^" ++ show i

instance (Eq a, Num a) => Num (UPoly a) where
    UP as + UP bs = toUPoly $ as <+> bs
    negate (UP as) = UP $ map negate as
    UP as * UP bs = toUPoly $ as <*> bs
    fromInteger 0 = UP []
    fromInteger a = UP [fromInteger a]
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

toUPoly as = UP (reverse (dropWhile (== 0) (reverse as)))

(a:as) <+> (b:bs) = (a+b) : (as <+> bs)
as <+> [] = as
[] <+> bs = bs

[] <*> _ = []
_ <*> [] = []
(a:as) <*> (b:bs) = [a*b] <+> (0 : map (a*) bs) <+> (0 : map (*b) as) <+> (0 : 0 : as <*> bs)


convert (UP as) = toUPoly $ map fromInteger as
-- Can be used with type annotations to construct polynomials over other types, eg
-- > convert (x^2+3*x+2) :: UPoly F2
-- x^2+x
-- > convert (x^2+3*x+2) :: UPoly F3
-- x^2+2


-- DIVISION ALGORITHM

-- degree
deg (UP as) = length as

-- leading term
lt (UP as) = last as

monomial a i = UP $ replicate i 0 ++ [a]

-- quotRem for UPolys over a field
quotRemUP :: (Eq k, Fractional k) => UPoly k -> UPoly k -> (UPoly k, UPoly k)
quotRemUP f g = qr 0 f where
    qr q r = if deg r < deg_g
             then (q,r)
             else let s = monomial (lt r / lt_g) (deg r - deg_g)
                  in qr (q+s) (r-s*g)
    deg_g = deg g
    lt_g = lt g


modUP f g = snd $ quotRemUP f g

-- extendedEuclidUP f g returns (u,v,d) such that u*f + v*g = d
extendedEuclidUP f g = extendedEuclidUP' f g [] where
    extendedEuclidUP' d 0 qs = let (u,v) = unwind 1 0 qs in (u,v,d)
    extendedEuclidUP' f g qs = let (q,r) = quotRemUP f g in extendedEuclidUP' g r (q:qs)
    unwind u v [] = (u,v)
    unwind u v (q:qs) = unwind v (u-v*q) qs


-- EXTENSION FIELDS

class PolynomialAsType k poly where
    pvalue :: (k,poly) -> UPoly k

data ExtensionField k poly = Ext (UPoly k) deriving (Eq,Ord)

instance (Eq k, Show k, Num k) => Show (ExtensionField k poly) where
    -- show (Ext f) = show f
    -- show (Ext (UP [])) = "0"
    show (Ext (UP as)) = showUP "a" as

instance (Eq k, Fractional k, PolynomialAsType k poly) => Num (ExtensionField k poly) where
    Ext x + Ext y = Ext $ (x+y) -- `modUP` pvalue (undefined :: (k,poly))
    Ext x * Ext y = Ext $ (x*y) `modUP` pvalue (undefined :: (k,poly))
    negate (Ext x) = Ext $ negate x
    fromInteger x = Ext $ fromInteger x
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance (Eq k, Fractional k, PolynomialAsType k poly) => Fractional (ExtensionField k poly) where
    recip 0 = error "ExtensionField.recip 0"
    recip (Ext f) = let g = pvalue (undefined :: (k,poly))
                        (u,v,d@(UP [c])) = extendedEuclidUP f g
                        -- so u*f + v*g == d. We know the d is a unit, ie field element, since g is irreducible
                    in Ext $ (c /> u) `modUP` g
    fromRational q = fromInteger a / fromInteger b where a = numerator q; b = denominator q

-- divide through
c /> f@(UP as) | c == 1 = f
               | c /= 0 = UP (map (c' *) as) where c' = recip c

instance (FiniteField k, PolynomialAsType k poly) => FiniteField (ExtensionField k poly) where
    eltsFq _ = map Ext (polys (d-1) fp) where
        fp = eltsFq (undefined :: k)
        d = deg $ pvalue (undefined :: (k,poly))
    basisFq _ = map embed $ take (d-1) $ iterate (*x) 1 where
        d = deg $ pvalue (undefined :: (k,poly))

-- Not sure if Eq fp is required, need to check with ghc >= 7.4.1
instance (FinSet fp, Eq fp, Num fp, PolynomialAsType fp poly) => FinSet (ExtensionField fp poly) where
    elts = map Ext (polys (d-1) fp') where
        fp' = elts
        d = deg $ pvalue (undefined :: (fp,poly))

embed f = Ext (convert f)


-- PRIME POWER FINITE FIELDS

polys d fp = map toUPoly $ polys' d where
    polys' 0 = [[]]
    polys' d = [x:xs | x <- fp, xs <- polys' (d-1)] -- return in ascending order
    -- polys' d = [x:xs | xs <- polys' (d-1), x <- fp] -- return with elts of fp first

-- Conway polynomials from Holt, Handbook of Computational Group Theory, p60

data ConwayF4
instance PolynomialAsType F2 ConwayF4 where pvalue _ = convert $ x^2+x+1
type F4 = ExtensionField F2 ConwayF4
f4 = map Ext (polys 2 f2) :: [F4]
a4 = embed x :: F4

data ConwayF8
instance PolynomialAsType F2 ConwayF8 where pvalue _ = convert $ x^3+x+1
type F8 = ExtensionField F2 ConwayF8
f8 = map Ext (polys 3 f2) :: [F8]
a8 = embed x :: F8

data ConwayF9
instance PolynomialAsType F3 ConwayF9 where pvalue _ = convert $ x^2+2*x+2
type F9 = ExtensionField F3 ConwayF9
f9 = map Ext (polys 2 f3) :: [F9]
a9 = embed x :: F9

data ConwayF16
instance PolynomialAsType F2 ConwayF16 where pvalue _ = convert $ x^4+x+1
type F16 = ExtensionField F2 ConwayF16
f16 = map Ext (polys 4 f2) :: [F16]
a16 = embed x :: F16

data ConwayF25
instance PolynomialAsType F5 ConwayF25 where pvalue _ = convert $ x^2+4*x+2
type F25 = ExtensionField F5 ConwayF25
f25 = map Ext (polys 2 f5) :: [F25]
a25 = embed x :: F25

data ConwayF27
instance PolynomialAsType F3 ConwayF27 where pvalue _ = convert $ x^3+2*x+1
type F27 = ExtensionField F3 ConwayF27
f27 = map Ext (polys 3 f3) :: [F27]
a27 = embed x :: F27

data ConwayF32
instance PolynomialAsType F2 ConwayF32 where pvalue _ = convert $ x^5+x^2+1
type F32 = ExtensionField F2 ConwayF32
f32 = map Ext (polys 5 f2) :: [F32]
a32 = embed x :: F32


-- generator for the automorphism group of fq, as applied to an element of fq
frobeniusAut x = x ^ p where
    p = char $ eltsFq x

-- the degree of fq as an extension over fp
-- (hence also, the order of the automorphism group of fq)
degree fq = n where
    q = length fq
    p = char fq
    Just n = L.elemIndex q $ iterate (*p) 1



-- QUADRATIC EXTENSIONS OF Q

data Sqrt a = Sqrt a

-- n should be square-free
instance IntegerAsType n => PolynomialAsType Q (Sqrt n) where
    pvalue _ = convert $ x^2 - fromInteger (value (undefined :: n))

type QSqrt2 = ExtensionField Q (Sqrt T2)
sqrt2 = embed x :: QSqrt2

type QSqrt3 = ExtensionField Q (Sqrt T3)
sqrt3 = embed x :: QSqrt3

type QSqrt5 = ExtensionField Q (Sqrt T5)
sqrt5 = embed x :: QSqrt5

type QSqrt7 = ExtensionField Q (Sqrt T7)
sqrt7 = embed x :: QSqrt7

type QSqrtMinus1 = ExtensionField Q (Sqrt TMinus1)
i = embed x :: QSqrtMinus1


type QSqrtMinus2 = ExtensionField Q (Sqrt (M TMinus1 T2))
sqrtminus2 = embed x :: QSqrtMinus2

type QSqrtMinus3 = ExtensionField Q (Sqrt (M TMinus1 T3))
sqrtminus3 = embed x :: QSqrtMinus3

type QSqrtMinus5 = ExtensionField Q (Sqrt (M TMinus1 T5))
sqrtminus5 = embed x :: QSqrtMinus5


-- conjugation automorphism of quadratic field
-- conjugate of a + b sqrt d is a - b sqrt d
conjugate :: ExtensionField Q (Sqrt d) -> ExtensionField Q (Sqrt d)
conjugate (Ext (UP [a,b])) = Ext (UP [a,-b])
conjugate x = x -- the zero or constant cases