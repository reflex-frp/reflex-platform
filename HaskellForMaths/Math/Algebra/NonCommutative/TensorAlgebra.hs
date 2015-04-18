-- Copyright (c) 2008, David Amos. All rights reserved.

-- |A module defining the tensor, symmetric, and exterior algebras.
-- This module has been partially superceded by Math.Algebras.TensorAlgebra, which should be used in preference.
-- This module is likely to be removed at some point.
module Math.Algebra.NonCommutative.TensorAlgebra where

import Math.Algebra.Field.Base
import Math.Algebra.NonCommutative.NCPoly hiding (X)
import Math.Algebra.NonCommutative.GSBasis


-- TENSOR ALGEBRA
-- Tensor product satisfies the universal property that any multilinear map from the cartesian product can be factored through the tensor product

-- The tensor algebra is the free algebra on the basis elts of the vector space

data Basis = E Int deriving (Eq,Ord)

instance Show Basis where
    show (E i) = 'e': show i

e_ i = NP [(M [E i], 1)] :: NPoly Q Basis

e1 = e_ 1
e2 = e_ 2
e3 = e_ 3
e4 = e_ 4

-- given an elt of the tensor algebra, return the dimension of the vector space it's defined over
dim (NP ts) = maximum $ 0 : [i | (M bs,c) <- ts, E i <- bs]

-- Monomial basis for tensor algebra over k^n - infinite
tensorBasis n = mbasisQA [e_ i | i <- [1..n]] []


-- EXTERIOR ALGEBRA
-- Exterior product satisfies the universal property that any alternating multilinear map from the cartesian product can be factored through the exterior product

-- Exterior algebra over k^n is tensor algebra over k^n quotiented by these relations
extRelations n = [e_ i * e_ i | i <- [1..n] ] ++
                 [e_ i * e_ j + e_ j * e_ i | i <- [1..n], j <- [i+1..n] ]

extnf t = t %% (extRelations $ dim t)

-- Monomial basis for exterior algebra over k^n - finite
exteriorBasis n = mbasisQA [e_ i | i <- [1..n]] $ extRelations n


-- SYMMETRIC ALGEBRA
-- Symmetric product satisfies the universal property that any symmetric multilinear map from the cartesian product can be factored through the symmetric product

-- Symmetric algebra over k^n is tensor algebra over k^n quotiented by these relations
symRelations n = [e_ i * e_ j - e_ j * e_ i | i <- [1..n], j <- [i+1..n] ]

symnf t = t %% (symRelations $ dim t)

-- Monomial basis for symmetric algebra over k^n - infinite
symmetricBasis n = mbasisQA [e_ i | i <- [1..n]] $ symRelations n


-- WEYL ALGEBRAS
-- http://en.wikipedia.org/wiki/Weyl_algebra
-- Coutinho, A Primer of Algebraic D-modules, ch1

-- Given a symplectic form w, represented by
-- [0  I]
-- [-I 0]
-- on R^2n
-- The Weyl algebra is the tensor algebra quotiented by < u*v-v*u-w(u,v) >
-- It has a natural interpretation as an operator algebra in which
-- e_1 .. e_i .. e_n correspond to x_i (the "multiply by x_i" operator),
-- e_n+1 .. e_n+i .. e_2*n correspond to d_x_i (the "differentiate wrt x_i" operator)

-- Weyl algebra W(V) is a "quantization" of the Symmetric algebra Sym(V)

weylRelations n = [e_ j * e_ i - e_ i * e_ j | i <- [1..2*n], j <- [i+1..2*n], j /= i+n ] ++
                  [e_ (i+n) * e_ i - e_ i * e_ (i+n) - 1 | i <- [1..n] ]

weylnf n t = t %% (weylRelations n)

weylBasis n = mbasisQA [e_ i | i <- [1..2*n]] $ weylRelations n


-- Explicit construction of Weyl algebra in terms of d_x_i and x_i operators

data WeylGens = X Int | D Int deriving (Eq,Ord)

instance Show WeylGens where
    show (D i) = 'd': show i
    show (X i) = 'x': show i

d_ i = NP [(M [D i], 1)] :: NPoly Q WeylGens
x_ i = NP [(M [X i], 1)] :: NPoly Q WeylGens

d1 = d_ 1
d2 = d_ 2
d3 = d_ 3

x1 = x_ 1
x2 = x_ 2
x3 = x_ 3

comm p q = p*q - q*p

delta i j = if i == j then 1 else 0

weylRelations' n = [comm (x_ i) (x_ j) | i <- [1..n], j <- [i+1..n] ] ++
                   [comm (d_ i) (d_ j) | i <- [1..n], j <- [i+1..n] ] ++
                   [comm (d_ i) (x_ j) - delta i j | i <- [1..n], j <- [1..n] ]

weylnf' f@(NP ts) = f %% weylRelations' n where
    n = maximum $ 0 : [i | (M bs,c) <- ts, X i <- bs] ++ [i | (M bs,c) <- ts, D i <- bs]

weylBasis' n = mbasisQA (map x_ [1..n] ++ map d_ [1..n]) (weylRelations' n)

{-
-- HEISENBERG ALGEBRA

data Heisenberg = D | U deriving (Eq,Ord)

instance Show Heisenberg where
    show D = "d"
    show U = "u"

d = NP [(M [D], 1)] :: NPoly Q Heisenberg
u = NP [(M [U], 1)] :: NPoly Q Heisenberg


heisenberg = [u*d-d*u-1]


-- Monomial basis for Heisenberg algebra - infinite
hBasis = mbasisQA [d,u] (gb heisenberg)
-}