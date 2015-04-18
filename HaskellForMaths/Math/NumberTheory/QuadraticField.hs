-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

-- |A module for arithmetic in quadratic number fields. A quadratic number field is a field of the form Q(sqrt d),
-- where d is a square-free integer. For example, we can perform the following calculation in Q(sqrt 2):
--
-- > (1 + sqrt 2) / (2 + sqrt 2)
--
-- It is also possible to mix different square roots in the same calculation. For example:
--
-- > (1 + sqrt 2) * (1 + sqrt 3)
--
-- Square roots of negative numbers are also permitted. For example:
--
-- > i * sqrt(-3)
module Math.NumberTheory.QuadraticField where

import Prelude hiding (sqrt)

import Data.List as L
import Math.Core.Field
import Math.Core.Utils (powersetdfs)
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures
import Math.NumberTheory.Factor

import Math.Algebra.LinearAlgebra hiding (inverse, (*>) )
import Math.CommutativeAlgebra.Polynomial


-- Q(sqrt n)

-- |A basis for quadratic number fields Q(sqrt d), where d is a square-free integer.
data QNFBasis = One | Sqrt Integer deriving (Eq,Ord)

instance Show QNFBasis where
    show One = "1"
    show (Sqrt d) | d == -1 = "i"
                  | otherwise = "sqrt(" ++ show d ++ ")"

-- |The type for elements of quadratic number fields
type QNF = Vect Q QNFBasis

-- |Although this has the same name as the Prelude.sqrt function, it should be thought of as more like a constructor
-- for creating elements of quadratic fields.
--
-- Note that for d positive, sqrt d means the positive square root, and sqrt (-d) should be interpreted as the square root
-- with positive imaginary part, that is i * sqrt d. This has the consequence that for example, sqrt (-2) * sqrt (-3) = - sqrt 6.
sqrt :: Integer -> QNF
sqrt d | fr == 1   = fromInteger sq
       | otherwise = fromInteger sq * return (Sqrt fr)
    where (sq,fr) = squaredFree 1 1 (pfactors d)
          squaredFree squared free (d1:d2:ds) =
              if d1 == d2 then squaredFree (d1*squared) free ds else squaredFree squared (d1*free) (d2:ds)
          squaredFree squared free ds = (squared, free * product ds)

sqrt2 = sqrt 2
sqrt3 = sqrt 3
sqrt5 = sqrt 5
sqrt6 = sqrt 6
sqrt7 = sqrt 7

i :: QNF
i = sqrt (-1)

instance (Eq k, Num k) => Algebra k QNFBasis where
    unit x = x *> return One
    mult = linear mult'
         where mult' (One,x) = return x
               mult' (x,One) = return x
               mult' (Sqrt m, Sqrt n) | m == n = unit (fromInteger m)
                                      | otherwise = let (i,d) = interdiff (pfactors m) (pfactors n) 1 1
                                                    in fromInteger i *> return (Sqrt d)
               -- if squarefree a == product ps, b == product qs
               -- then sqrt a * sqrt b = product (intersect ps qs) * sqrt (product (symdiff ps qs))
               -- the following calculates these two products
               -- in particular, it correctly handles the case that either or both contain -1
               interdiff (p:ps) (q:qs) i d =
                   case compare p q of
                   LT -> interdiff ps (q:qs) i (d*p)
                   EQ -> interdiff ps qs (i*p) d
                   GT -> interdiff (p:ps) qs i (d*q)
               interdiff ps qs i d = (i, d * product (ps ++ qs))

{-
instance HasConjugation Q QNFBasis where
    conj = (>>= conj') where
        conj' One = return One
        conj' sqrt_d = -1 *> return sqrt_d
    -- ie conj = linear conj', but avoiding unnecessary nf call
    sqnorm x = coeff One (x * conj x)
-}

newtype XVar = X Int deriving (Eq, Ord, Show)

instance Fractional QNF where
    recip x@(V ts) =
        let ds = [d | (Sqrt d, _) <- terms x]
            fs = (if any (<0) ds then [-1] else []) ++ pfactors (foldl lcm 1 ds) -- lcm is always positive
            rs = map (\d -> case d of 1 -> One; d' -> Sqrt d') $
                 map product $ powersetdfs $ fs 
            -- for example, for x == sqrt2 + sqrt3, we would have rs == [One, Sqrt 2, Sqrt 3, Sqrt 6]
            n = length rs
            y = V $ zip rs $ map (glexvar . X) [1..n] -- x1*1+x2*r2+...+xn*rn
            x' = V $ map (\(s,c) -> (s, unit c)) ts -- lift the coefficients in x into the polynomial algebra
            one = x' * y
            m = [ [coeff (mvar (X j)) c | j <- [1..n]] | i <- rs, let c = coeff i one] -- matrix of the linear system
            b = 1 : replicate (n-1) 0
        in case solveLinearSystem m b of -- find v such that m v == b - ie find the values of x1, x2, ... xn
            Just v -> nf $ V $ zip rs v
            Nothing -> error "QNF.recip 0"
    fromRational q = fromRational q *> 1


