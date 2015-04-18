-- Copyright (c) David Amos, 2008. All rights reserved.

module Math.Algebra.NonCommutative.GSBasis where

import Data.List as L

import Math.Algebra.NonCommutative.NCPoly


-- given two monomials f g, find if possible a,b,c with f=ab g=bc
findOverlap (M xs) (M ys) = findOverlap' [] xs ys where
    findOverlap' as [] cs = Nothing -- (reverse as, [], cs)
    findOverlap' as (b:bs) cs =
        if (b:bs) `L.isPrefixOf` cs
        then Just (M $ reverse as, M $ b:bs, M $ drop (length (b:bs)) cs)
        else findOverlap' (b:as) bs cs

-- given two monomials f g, find if possible l,r with g = lfr
-- findInclusion (M xs) (M ys) = findInclusion' 

sPoly f@(NP ((xs,c):_)) g@(NP ((ys,d):_)) =
    case findOverlap xs ys of
    Just (l,m,r) -> f * NP [(r,d)] - NP [(l,c)] * g
    Nothing -> 0
sPoly _ _ = 0 -- !! shouldn't reach this
-- The point about the s-poly is that it cancels out the leading terms of the two polys, exposing their second terms


gb1 fs = gb' fs [sPoly fi fj | fi <- fs, fj <- fs, fi /= fj] where -- unlike the commutative case, we take sPolys both ways round
    gb' gs (h:hs) = let h' = h %% gs in
                    if h' == 0 then gb' gs hs else gb' (h':gs) (hs ++ [sPoly h' g | g <- gs] ++ [sPoly g h' | g <- gs])
    gb' gs [] = gs

reduce gs = reduce' [] gs where
    reduce' gs' (g:gs) | g' == 0   = reduce' gs' gs
                       | otherwise = reduce' (g':gs') gs
                       where g' = g %% (gs'++gs)
    reduce' gs' [] = reverse $ sort $ gs'

gb fs = map toMonic $ reduce $ gb1 fs

gb' fs = reduce $ gb1 fs


gb2 fs = gb' fs [(fi,fj) | fi <- fs, fj <- fs, fi /= fj] where -- unlike the commutative case, we take sPolys both ways round
    gb' gs ((fi,fj):pairs) =
        let h = sPoly fi fj %% gs in
        if h == 0 then gb' gs pairs else gb' (h:gs) (pairs ++ [(h,g) | g <- gs] ++ [(g,h) | g <- gs])
    gb' gs [] = gs

gb2' fs = gb' fs [(fi,fj) | fi <- fs, fj <- fs, fi /= fj] where -- unlike the commutative case, we take sPolys both ways round
    gb' gs ((fi,fj):pairs) =
        let h = sPoly fi fj %% gs in
        if h == 0 then gb' gs pairs else (fi,fj,sPoly fi fj,h) : gb' (h:gs) (pairs ++ [(h,g) | g <- gs] ++ [(g,h) | g <- gs])
    gb' gs [] = [] -- gs


-- Monomial basis for the quotient algebra, where gs are the generators, rs the relations
mbasisQA gs rs = mbasisQA' [1] where
    mbasisQA' [] = [] -- the quotient ring has a finite monomial basis
    mbasisQA' ms = let ms' = [g*m | g <- gs, m <- ms, g*m %% rs == g*m] -- ie, not reducible
                   in ms ++ mbasisQA' ms'
{-
isGB fs = all (\h -> h %% fs == 0) (pairWith sPoly fs)
-}

