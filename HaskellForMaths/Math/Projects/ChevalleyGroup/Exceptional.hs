-- Copyright (c) 2008, David Amos. All rights reserved.

module Math.Projects.ChevalleyGroup.Exceptional where

import Data.List as L

-- import Math.Algebra.Field.Base
-- import Math.Algebra.Field.Extension hiding ( (<+>), (<*>) )
import Math.Core.Field
import Math.Algebra.LinearAlgebra

import Math.Algebra.Group.PermutationGroup hiding (fromList)
-- import Math.Algebra.Group.SchreierSims as SS
import Math.Algebra.Group.RandomSchreierSims as RSS

import Math.Combinatorics.FiniteGeometry (ptsAG)


-- Follows Conway's notation

-- The octonion xinf + x0 i0 + x1 i1 + ... + x6 i6
-- is represented as O [(-1,xinf),(0,x0),(1,x1),...,(6,x6)]
-- where a list element may be omitted if the coefficient is zero


newtype Octonion k = O [(Int,k)] deriving (Eq, Ord)

i0, i1, i2, i3, i4, i5, i6 :: Octonion Q
i0 = O [(0,1)]
i1 = O [(1,1)]
i2 = O [(2,1)]
i3 = O [(3,1)]
i4 = O [(4,1)]
i5 = O [(5,1)]
i6 = O [(6,1)]

fromList as = O $ filter ((/=0) . snd) $ zip [-1..6] as

toList (O xs) = toList' xs [-1..6] where
    toList' ((i,a):xs) (j:js) =
        if i == j then a : toList' xs js else 0 : toList' ((i,a):xs) js
    toList' [] (j:js) = 0 : toList' [] js
    toList' _ [] = []

expose (O ts) = ts

instance Show k => Show (Octonion k) where
    show (O []) = "0"
    show (O ts) = let c:cs = concatMap showTerm ts
                  in if c == '+' then cs else c:cs
        where showTerm (i,a) = showCoeff a ++ showImag a i
              showCoeff a = case show a of
                            "1" -> "+"
                            "-1" -> "-"
                            '-':cs -> '-':cs
                            cs -> '+':cs
              showImag a i | i == -1 = case show a of
                                       "1" -> "1"
                                       "-1" -> "1"
                                       otherwise -> ""
                           | otherwise = "i" ++ show i

instance (Ord k, Num k) => Num (Octonion k) where -- Ord k not strictly required, but keeps nf simpler
    O ts + O us = O $ nf $ ts ++ us
    negate (O ts) = O $ map (\(i,a) -> (i,-a)) ts
    O ts * O us = O $ nf [m t u | t <- ts, u <- us]
    fromInteger 0 = O []
    fromInteger n = O [(-1, fromInteger n)]

nf ts = nf' $ L.sort ts where
    nf' ((i1,a1):(i2,a2):ts) =
        if i1 == i2
        then if a1+a2 == 0 then nf' ts else nf' ((i1,a1+a2):ts)
        else (i1,a1) : nf' ((i2,a2):ts)
    nf' ts = ts

m (-1,a) (i,b) = (i,a*b)
m (i,a) (-1,b) = (i,a*b)
m (i,a) (j,b) =
    case (j-i) `mod` 7 of
    0 -> (-1,-a*b)
    1 -> ( (i+3) `mod` 7, a*b)	-- i_n+1 * i_n+2 == i_n+4
    2 -> ( (i+6) `mod` 7, a*b)	-- i_n+2 * i_n+4 == i_n+1
    3 -> ( (i+1) `mod` 7, -a*b)	-- i_n+1 * i_n+4 == -i_n+2
    4 -> ( (i+5) `mod` 7, a*b)	-- i_n+4 * i_n+1 == i_n+2
    5 -> ( (i+4) `mod` 7, -a*b)	-- i_n+4 * i_n+2 == -i_n+1
    6 -> ( (i+2) `mod` 7, -a*b)	-- i_n+2 * i_n+1 == -i_n+4


conj (O ts) = O $ map (\(i,a) -> if i == -1 then (i,a) else (i,-a)) ts

sqnorm (O ts) = sum [a^2 | (i,a) <- ts]

instance (Ord k, Num k, Fractional k) => Fractional (Octonion k) where
    recip x = let O x' = conj x
                  xx' = sqnorm x
              in O $ map (\(i,a) -> (i,a/xx')) x'


isOrthogonal (O ts) (O us) = dot ts us == 0 where
    dot ((i,a):ts) ((j,b):us) =
        case compare i j of
        EQ -> a*b + dot ts us
        LT -> dot ts ((j,b):us)
        GT -> dot ((i,a):ts) us
    dot _ _ = 0

antiCommutes x y = x*y + y*x == 0

-- anti-commuting and being orthogonal appear to be equivalent for unit imaginary octonions,
-- provided we're not in characteristic 2


-- OCTONIONS OVER FINITE FIELDS

{-
octonions fq = map O $ octonions' [-1..6] where
    octonions' (i:is) = [if a == 0 then ts else (i,a):ts | a <- fq, ts <- octonions' is]
    octonions' [] = [[]]
-}
octonions fq = map fromList $ ptsAG 8 fq

isUnit x = sqnorm x == 1

unitImagOctonions fq = filter isUnit $ map (fromList . (0:)) $ ptsAG 7 fq


-- given the images of i0, i1, i2, return the automorphism
-- the inputs must be pure imaginary unit octonions
-- and we must have isOrthogonal i0 i1, isOrthogonal i0 i2, isOrthogonal i1 i2, and isOrthogonal (i0*i1) i2
autFrom i0' i1' i2' =
    let 0:r0 = toList i0'
        0:r1 = toList i1'
        0:r2 = toList i2'
        0:r3 = toList $ i0'*i1'
        0:r4 = toList $ i1'*i2'
        0:r5 = toList $ i0'*(i1'*i2')
        0:r6 = toList $ i0'*i2'
    in [r0,r1,r2,r3,r4,r5,r6]


x %^ g =
    let a:as = toList x
    in fromList $ a : (as <*>> g)


-- G2(3)

alpha3 = autFrom (O [(1,1::F3)]) (O [(2,1)]) (O [(3,1)])
beta3 = autFrom (O [(0,1::F3)]) (O [(2,1)]) (O [(4,1)])

gamma3s = [x | x <- unitImagOctonions f3, isOrthogonal (O [(0,1)]) x, isOrthogonal (O [(1,1)]) x, isOrthogonal (O [(3,1)]) x]

gamma3 = autFrom (O [(0,1::F3)]) (O [(1,1)]) (O [(2,1),(4,1),(5,1),(6,1)])

alpha3' = fromPairs [(x, x %^ alpha3) | x <- unitImagOctonions f3]
beta3' = fromPairs [(x, x %^ beta3) | x <- unitImagOctonions f3]
gamma3' = fromPairs [(x, x %^ gamma3) | x <- unitImagOctonions f3]

-- |Generators for G2(3), a finite simple group of order 4245696,
-- as a permutation group on the 702 unit imaginary octonions over F3
g2_3 :: [Permutation (Octonion F3)]
g2_3 = [alpha3', beta3', gamma3']
-- These three together generate a group of order 4245696, which is therefore the whole of G2(3)

-- Unit imaginary octonions form one orbit under the action of G2

-- [alpha', beta', gamma' generate G2(3) as a permutation group on 702 points (the number of unit imaginary octonions over F3)
-- Interestingly, http://brauer.maths.qmul.ac.uk/Atlas/v3/exc/G23/ doesn't seem to have this permutation representation


-- G2(4)

alpha4 = autFrom (O [(1,1::F4)]) (O [(2,1)]) (O [(3,1)])
beta4 = autFrom (O [(0,1::F4)]) (O [(2,1)]) (O [(4,1)])

gamma4s = [x | x <- unitImagOctonions f4, isOrthogonal (O [(0,1)]) x, isOrthogonal (O [(1,1)]) x, isOrthogonal (O [(3,1)]) x]

-- gamma4 = autFrom (O [(0,1::F4)]) (O [(1,1)]) (O [(5,embed x),(6,embed $ 1+x)])
gamma4 = autFrom (O [(0,1::F4)]) (O [(1,1)]) (O [(5,a4),(6,1+a4)])

alpha4' = fromPairs [(x, x %^ alpha4) | x <- unitImagOctonions f4]
beta4' = fromPairs [(x, x %^ beta4) | x <- unitImagOctonions f4]
gamma4' = fromPairs [(x, x %^ gamma4) | x <- unitImagOctonions f4]

-- Haven't checked whether these generate whole group - can be expected to run a long time