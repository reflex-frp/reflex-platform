-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, NoMonomorphismRestriction #-}

-- |A module defining the (non-associative) algebra of octonions over an arbitrary field.
--
-- The octonions are the algebra defined by the basis {1,i0,i1,i2,i3,i4,i5,i6},
-- where each i_n * i_n = -1, and i_n+1 * i_n+2 = i_n+4 (where the indices are modulo 7).
module Math.Algebras.Octonions where

import Math.Core.Field
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct hiding (i1,i2)
import Math.Algebras.Structures
import Math.Algebras.Quaternions

import Math.Combinatorics.FiniteGeometry (ptsAG)


-- Conway & Smith, On Quaternions and Octonions

-- OCTONIONS

data OBasis = O Int deriving (Eq,Ord)
-- map (return . O) [-1..6] -> [1,i0,i1,i2,i3,i4,i5,i6]

type Octonion k = Vect k OBasis

instance Show OBasis where
    show (O n) | n == -1 = "1"
               | 0 <= n && n <= 6 = "i" ++ show n
               | otherwise = error "Octonion: invalid basis element"

i0, i1, i2, i3, i4, i5, i6 :: Octonion Q
i0 = return (O 0)
i1 = return (O 1)
i2 = return (O 2)
i3 = return (O 3)
i4 = return (O 4)
i5 = return (O 5)
i6 = return (O 6)

i_ :: Num k => Int -> Octonion k
i_ n = return (O n)

instance (Eq k, Num k) => Algebra k OBasis where
    unit x = x *> return (O (-1))
    mult = linear m where
        m (O (-1), O n) = return (O n)
        m (O n, O (-1)) = return (O n)
        m (O a, O b) = case (b-a) `mod` 7 of
                       0 -> -1
                       1 -> i_ ((a+3) `mod` 7)       -- i_n+1 * i_n+2 == i_n+4
                       2 -> i_ ((a+6) `mod` 7)       -- i_n+2 * i_n+4 == i_n+1
                       3 -> -1 *> i_ ((a+1) `mod` 7) -- i_n+1 * i_n+4 == -i_n+2
                       4 -> i_ ((a+5) `mod` 7)       -- i_n+4 * i_n+1 == i_n+2
                       5 -> -1 *> i_ ((a+4) `mod` 7) -- i_n+4 * i_n+2 == -i_n+1
                       6 -> -1 *> i_ ((a+2) `mod` 7) -- i_n+2 * i_n+1 == -i_n+4

instance (Eq k, Num k) => HasConjugation k OBasis where
    conj = (>>= conj') where
        conj' (O n) = (if n == -1 then 1 else -1) *> return (O n)
    -- ie conj = linear conj', but avoiding unnecessary nf call
    sqnorm x = sum $ map ((^2) . snd) $ terms x
    -- sqnorm x = scalarPart (x * conj x)
            
-- Hence, the octonions inherit a Fractional instance

-- octonions fq = [sum $ zipWith (\x n -> x *> i_ n) xs [-1..6] | xs <- ptsAG 8 fq]
