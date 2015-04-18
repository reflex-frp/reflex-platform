-- Copyright (c) David Amos, 2008. All rights reserved.

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Math.Projects.KnotTheory.Braid where

import Data.List ( (\\) )

import Math.Algebra.Field.Base
import Math.Algebra.NonCommutative.NCPoly

import Math.Projects.KnotTheory.LaurentMPoly

type LPQ = LaurentMPoly Q

instance Invertible LPQ where
    inv = recip


-- BRAID ALGEBRA

data BraidGens = S Int deriving (Eq,Ord)
-- Inverse of S n is S (-n)

instance Show BraidGens where
    show (S i) | i > 0 = 's': show i
               | i < 0 = 's': show (-i) ++ "'"

s_ i = NP [(M [S i], 1)] :: NPoly LPQ BraidGens

s1 = s_ 1
s2 = s_ 2
s3 = s_ 3
s4 = s_ 4

instance Invertible (NPoly LPQ BraidGens) where
    inv (NP [(M [S i], 1)]) = s_ (-i)

{-
braidRelations n =
    [s_ j * s_ i - s_ i * s_ j | i <- [1..n-1], j <- [i+2..n-1] ] ++
    [s_ (i+1) * s_ i * s_ (i+1) - s_ i * s_ (i+1) * s_ i | i <- [1..n-2] ]
-- !! need relations for the inverses too !!
-- (but we're not intending to work in the braid algebra - we're intending to map into Temperley-Lieb or Iwahori-Hecke)
-}

-- The writhe of a braid == the sum of the signs of the crossings
writhe (NP [(M xs,c)]) = sum [signum i | S i <- xs]




-- Some knots - Lickorish p5, p27
-- (Note: These knots/braids give the correct Homfly/Jones polynomials compared to Lickorish)
-- (In general, that's not sufficient to prove that they are the claimed knots, although in these cases, they are.)
k3_1 = s1^-3
k4_1 = s2^-1 * s1 * s2^-1 * s1
k5_1 = s1^-5
k7_1 = s1^-7
