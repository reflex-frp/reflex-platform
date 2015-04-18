-- Copyright (c) David Amos, 2011. All rights reserved.

{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}

-- |A module providing an efficient implementation of the Buchberger algorithm for calculating the (reduced) Groebner basis for an ideal,
-- together with some straightforward applications.
module Math.CommutativeAlgebra.GroebnerBasis where

import Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Set as S

import Math.Core.Utils
import Math.Core.Field
import Math.Algebras.VectorSpace
import Math.Algebras.Structures
import Math.CommutativeAlgebra.Polynomial

-- Sources:
-- Cox, Little, O'Shea: Ideals, Varieties and Algorithms
-- Giovini, Mora, Niesi, Robbiano, Traverso, "One sugar cube please, or Selection strategies in the Buchberger algorithm"


sPoly f g = let d = tgcd (lt f) (lt g)
            in (lt g `tdiv` d) *-> f - (lt f `tdiv` d) *-> g
-- The point about the s-poly is that it cancels out the leading terms of the two polys, exposing their second terms


isGB fs = all (\h -> h %% fs == 0) (pairWith sPoly fs)


-- Initial, naive version
-- Cox p87
gb1 fs = gb' fs (pairWith sPoly fs) where
    gb' gs (h:hs) = let h' = h %% gs in
                    if h' == 0 then gb' gs hs else gb' (h':gs) (hs ++ map (sPoly h') gs)
    gb' gs [] = gs

-- [f xi xj | xi <- xs, xj <- xs, i < j]
pairWith f (x:xs) = map (f x) xs ++ pairWith f xs
pairWith _ [] = []


-- Cox p89-90
reduce gs = reduce' gs [] where
    reduce' (l:ls) rs = let l' = l %% (rs ++ ls) in
                        if l' == 0 then reduce' ls rs else reduce' ls (toMonic l':rs)
    reduce' [] rs = L.sort rs
-- when using an elimination order, the elimination ideal will be at the end

-- Cox et al p106-7
-- No need to calculate an spoly fi fj if
-- 1. the lm fi and lm fj are coprime, or
-- 2. there exists some fk, with (i,k) (j,k) already considered, and lm fk divides lcm (lm fi) (lm fj) 
-- some slight inefficiencies from looking up fi, fj repeatedly
gb2 fs = reduce $ gb' fs (pairs [1..s]) s where
    s = length fs
    gb' gs ((i,j):ps) t =
        let fi = gs!i; fj = gs!j in
        if mcoprime (lm fi) (lm fj) || criterion fi fj
        then gb' gs ps t
        else let h = sPoly fi fj %% gs in
             if h == 0 then gb' gs ps t else gb' (gs++[h]) (ps ++ [ (i,t+1) | i <- [1..t] ]) (t+1)
        where
            criterion fi fj = let l = mlcm (lm fi) (lm fj) in any (test l) [1..t]
            test l k = k `notElem` [i,j]
                    && ordpair i k `notElem` ps
                    && ordpair j k `notElem` ps
                    && lm (gs!k) `mdivides` l
    gb' gs [] _ = gs

xs ! i = xs !! (i-1) -- in other words, index the list from 1 not 0


-- Doesn't result in any speedup
gb2a fs = reduce $ gb' fs' (pairs [1..s]) s
    where fs' = IM.fromList $ zip [1..] $ filter (/= 0) fs
          s = IM.size fs'
          gb' gs ((i,j):ps) t =
              let fi = gs IM.! i; fj = gs IM.! j in
              if mcoprime (lm fi) (lm fj) || criterion fi fj
              then gb' gs ps t
              else let h = sPoly fi fj %% IM.elems gs in
                   if h == 0
                   then gb' gs ps t
                   else let t' = t+1
                            gs' = IM.insert t' h gs
                            ps' = ps ++ map (,t') [1..t]
                        in gb' gs' ps' t'
              where criterion fi fj = let l = mlcm (lm fi) (lm fj) in any (test l) [1..t]
                    test l k = k `notElem` [i,j]
                            && ordpair i k `notElem` ps
                            && ordpair j k `notElem` ps
                            && lm (gs IM.! k) `mdivides` l
          gb' gs [] _ = IM.elems gs


-- version of gb2 where we eliminate pairs as they're created, rather than as they're processed
gb3 fs = reduce $ gb1' [] fs [] 0
    where
    gb1' gs (f:fs) ps t = let ps' = updatePairs gs ps f t
                          in gb1' (gs ++ [f]) fs ps' (t+1)
    gb1' ls [] ps t = gb2' ls ps t
    gb2' gs ((i,j):ps) t =
        let h = sPoly (gs!i) (gs!j) %% gs in
        if h == 0
        then gb2' gs ps t
        else let ps' = updatePairs gs ((i,j):ps) h t in gb2' (gs++[h]) ps' (t+1)
    gb2' gs [] _ = gs
    updatePairs gs ps f t =
        [p | p@(i,j) <- ps,
             not (lm f `mdivides` mlcm (lm (gs!i)) (lm (gs!j)))] 
     ++ [ (i,t+1) | (gi,i) <- zip gs [1..t],
                    not (mcoprime (lm gi) (lm f)),
                    not (criterion (mlcm (lm gi) (lm f)) i) ]
        where criterion l i = any (`mdivides` l) [lm gk | (gk,k) <- zip gs [1..t], k /= i, ordpair i k `notElem` ps]


-- Cox et al 108
-- 1. list smallest fs first, as more likely to reduce
-- 2. order the pairs with smallest lcm fi fj first ("normal selection strategy")
gb4 fs = reduce $ gb1' [] fs' [] 0
    where fs' = reverse $ L.sort $ filter (/=0) fs
          gb1' gs (f:fs) ps t = gb1' (gs ++ [f]) fs ps' (t+1)
              where ps' = updatePairs gs ps f t
          gb1' ls [] ps t = gb2' ls ps t
          gb2' gs ((l,(i,j)):ps) t =
              let h = sPoly (gs!i) (gs!j) %% gs in
              if h == 0
              then gb2' gs ps t
              else let ps' = updatePairs gs ((l,(i,j)):ps) h t in gb2' (gs++[h]) ps' (t+1)
          gb2' gs [] _ = gs
          updatePairs gs ps f t =
              let oldps = [p | p@(l,(i,j)) <- ps, not (lm f `mdivides` l)]
                  newps = sortBy (flip cmpfst)
                          [ (l,(i,t+1)) | (gi,i) <- zip gs [1..t], let l = mlcm (lm gi) (lm f),
                                          not (mcoprime (lm gi) (lm f)),
                                          not (criterion l i) ]
              in mergeBy (flip cmpfst) oldps newps
              where criterion l i = any (`mdivides` l) [lm gk | (gk,k) <- zip gs [1..t], k /= i, ordpair i k `notElem` map snd ps]


mergeBy cmp (t:ts) (u:us) =
    case cmp t u of
    LT -> t : mergeBy cmp ts (u:us)
    EQ -> t : mergeBy cmp ts (u:us)
    GT -> u : mergeBy cmp (t:ts) us
mergeBy _ ts us = ts ++ us -- one of them is null


-- Giovini et al
-- The point of sugar is, given fi, fj, to give an upper bound on the degree of sPoly fi fj without having to calculate it
-- We can then select by preference pairs with lower sugar, expecting therefore that the s-polys will have lower degree

-- It is only for Lex ordering that sugar seems to give a substantial improvement
-- gb4 is fine for Grevlex

-- !! can probably get rid of the Ord k requirement in the following

-- |Given a list of polynomials over a field, return a Groebner basis for the ideal generated by the polynomials.
gb :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
    [Vect k m] -> [Vect k m]
gb fs =
    let fs' = reverse $ sort $ map toMonic $ filter (/=0) fs
    in reduce $ gb1' [] fs' [] 0 where
    gb1' gs (f:fs) ps t = gb1' (gs ++ [f]) fs ps' (t+1)
        where ps' = updatePairs gs ps f (t+1)
    gb1' ls [] ps t = gb2' ls ps t
    gb2' gs (p@(_,(i,j)):ps) t =
        if h == 0
        then gb2' gs ps t
        else gb2' (gs++[h]) ps' (t+1)
        where h = toMonic $ sPoly (gs!i) (gs!j) %% gs
              ps' = updatePairs gs (p:ps) h (t+1)
    gb2' gs [] _ = gs
    updatePairs gs ps gk k =
        let newps = [let l = mlcm (lm gi) (lm gk) in ((sugar gi gk l, l), (i,k)) | (gi,i) <- zip gs [1..k-1] ]
            ps' = [p | p@((sij,tij),(i,j)) <- ps,
                       let ((sik,tik),_) = newps ! i, let ((sjk,tjk),_) = newps ! j,
                       not ( (tik `mproperlydivides` tij) && (tjk `mproperlydivides` tij) ) ] -- sloppy variant
            newps' = discard1 [] newps
            newps'' = sortBy (flip cmpSug) $ discard2 [] $ sortBy (flip cmpNormal) newps'
        in mergeBy (flip cmpSug) ps' newps''
        where
            discard1 ls (r@((_sik,tik),(i,_k)):rs) =
                if lm (gs!i) `mcoprime` lm gk
                -- then discard [l | l@((_,tjk),_) <- ls, tjk /= tik] [r | r@((_,tjk),_) <- ls, tjk /= tik]
                then discard1 (filter (\((_,tjk),_) -> tjk /= tik) ls) (filter (\((_,tjk),_) -> tjk /= tik) rs)
                else discard1 (r:ls) rs
            discard1 ls [] = ls
            discard2 ls (r@((_sik,tik),(i,k)):rs) = discard2 (r:ls) $ filter (\((_sjk,tjk),(j,k')) -> not (k == k' && tik `mdivides` tjk)) rs
            discard2 ls [] = ls
-- The two calls to toMonic are designed to prevent coefficient explosion, but it is unproven that they are effective

-- sugar of sPoly f g, where h = lcm (lt f) (lt g)
-- this is an upper bound on deg (sPoly f g)
sugar f g h = mdeg h + max (deg f - mdeg (lm f)) (deg g - mdeg (lm g))

cmpNormal ((s1,t1),(i1,j1)) ((s2,t2),(i2,j2)) = compare (t1,j1) (t2,j2)

cmpSug ((s1,t1),(i1,j1)) ((s2,t2),(i2,j2)) = compare (-s1,t1,j1) (-s2,t2,j2)


{-
merge (t:ts) (u:us) =
    if t <= u
    then t : merge ts (u:us)
    else u : merge (t:ts) us
merge ts us = ts ++ us -- one of them is null
-}

-- OPERATIONS ON IDEALS

memberGB f gs = f %% gs == 0

-- |@memberI f gs@ returns whether f is in the ideal generated by gs
memberI :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
    Vect k m -> [Vect k m] -> Bool
memberI f gs = memberGB f (gb gs)

-- Cox et al, p181
-- |Given ideals I and J, their sum is defined as I+J = {f+g | f \<- I, g \<- J}.
--
-- If fs and gs are generators for I and J, then @sumI fs gs@ returns generators for I+J.
--
-- The geometric interpretation is that the variety of the sum is the intersection of the varieties,
-- ie V(I+J) = V(I) intersect V(J)
sumI :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
     [Vect k m] -> [Vect k m] -> [Vect k m]
sumI fs gs = gb (fs ++ gs)

-- Cox et al, p183
-- |Given ideals I and J, their product I.J is the ideal generated by all products {f.g | f \<- I, g \<- J}.
--
-- If fs and gs are generators for I and J, then @productI fs gs@ returns generators for I.J.
--
-- The geometric interpretation is that the variety of the product is the union of the varieties,
-- ie V(I.J) = V(I) union V(J)
productI :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
     [Vect k m] -> [Vect k m] -> [Vect k m]
productI fs gs = gb [f * g | f <- fs, g <- gs]

-- Cox et al, p185-6
-- |The intersection of ideals I and J is the set of all polynomials which belong to both I and J.
--
-- If fs and gs are generators for I and J, then @intersectI fs gs@ returns generators for the intersection of I and J
--
-- The geometric interpretation is that the variety of the intersection is the union of the varieties,
-- ie V(I intersect J) = V(I) union V(J).
--
-- The reason for prefering the intersection over the product is that the intersection of radical ideals is radical,
-- whereas the product need not be.
intersectI :: (Fractional k, Ord k, Monomial m, Ord m) =>
     [Vect k m] -> [Vect k m] -> [Vect k m]
intersectI fs gs =
    let t = toElimFst $ return $ (mvar "t" :: Glex String)
        hs = map ((t *) . toElimSnd) fs ++ map (((1-t) *) . toElimSnd) gs
    in eliminateFst hs

toElimFst = fmap (\m -> Elim2 m munit)
toElimSnd = fmap (\m -> Elim2 munit m)
isElimFst = (/= munit) . (\(Elim2 m _) -> m) . lm
fromElimSnd = fmap (\(Elim2 _ m) -> m)
eliminateFst = map fromElimSnd . dropWhile isElimFst . gb


-- Cox et al, p193-4
-- |Given ideals I and J, their quotient is defined as I:J = {f | f \<- R, f.g is in I for all g in J}.
--
-- If fs and gs are generators for I and J, then @quotientI fs gs@ returns generators for I:J.
--
-- The ideal quotient is the algebraic analogue of the Zariski closure of a difference of varieties.
-- V(I:J) contains the Zariski closure of V(I)-V(J), with equality if k is algebraically closed and I is a radical ideal.
quotientI :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
    [Vect k m] -> [Vect k m] -> [Vect k m]
quotientI _ [] = [1]
quotientI fs gs = foldl1 intersectI $ map (quotientP fs) gs
-- quotientI fs gs = foldl intersectI [1] $ map (quotientP fs) gs

quotientP fs g = map ( // g ) $ intersectI fs [g]
    where h // g = let ([u],_) = quotRemMP h [g] in u

-- |@eliminate vs gs@ returns the elimination ideal obtained from the ideal generated by gs by eliminating the variables vs.
eliminate :: (Eq k, Fractional k, Ord k, MonomialConstructor m, Monomial (m v), Ord (m v)) =>
    [Vect k (m v)] -> [Vect k (m v)] -> [Vect k (m v)]
eliminate vs gs = let subs = subFst vs in eliminateFst [g `bind` subs | g <- gs]
    where subFst :: (Eq k, Num k, MonomialConstructor m, Eq (m v), Mon (m v)) =>
              [Vect k (m v)] -> v -> Vect k (Elim2 (m v) (m v))
          subFst vs = (\v -> let v' = var v in if v' `elem` vs then toElimFst v' else toElimSnd v')

{-
-- !! NOT WORKING
-- |@elimExcept vs gs@ returns the elimination ideal obtained from the ideal generated by gs by eliminating all variables except vs.
elimExcept :: (Fractional k, Ord k, MonomialConstructor m, Monomial (m v), Ord (m v)) =>
    [Vect k (m v)] -> [Vect k (m v)] -> [Vect k (m v)]
elimExcept vs gs = let subs = subSnd vs in eliminateFst [g `bind` subs | g <- gs]
    where subSnd :: (Num k, MonomialConstructor m, Eq (m v), Mon (m v)) =>
              [Vect k (m v)] -> v -> Vect k (Elim2 (m v) (m v))
          subSnd vs = (\v -> let v' = var v in if v' `elem` vs then toElimSnd v' else toElimFst v')
-}

-- MONOMIAL BASES FOR QUOTIENT ALGEBRAS

-- basis for the polynomial ring in variables vs
mbasis vs = mbasis' [1]
    where mbasis' ms = ms ++ mbasis' (toSet [v*m | v <- vs, m <- ms])

-- |Given variables vs, and a Groebner basis gs, @mbasisQA vs gs@ returns a monomial basis for the quotient algebra k[vs]/\<gs\>.
-- For example, @mbasisQA [x,y] [x^2+y^2-1]@ returns a monomial basis for k[x,y]/\<x^2+y^2-1\>.
-- In general, the monomial basis is likely to be infinite.
mbasisQA :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
     [Vect k m] -> [Vect k m] -> [Vect k m]
mbasisQA vs gs = mbasisQA' [1]
    where mbasisQA' [] = []  -- the quotient algebra is finite-dimensional
          mbasisQA' ms = ms ++ mbasisQA' (toSet [f | v <- vs, m <- ms, let f = v*m, f %% gs == f])

-- |Given an ideal I, the leading term ideal lt(I) consists of the leading terms of all elements of I.
-- If I is generated by gs, then @ltIdeal gs@ returns generators for lt(I).
ltIdeal :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
    [Vect k m] -> [Vect k m]
ltIdeal gs = map (return . lm) $ gb gs

-- number of monomials of degree i in n variables
numMonomials n i = toInteger (i+n-1) `choose` toInteger (n-1)

-- |Given variables vs, and a homogeneous ideal gs, @hilbertFunQA vs gs@ returns the Hilbert function for the quotient algebra k[vs]/\<gs\>.
-- Given an integer i, the Hilbert function returns the number of degree i monomials in a basis for k[vs]/\<gs\>.
-- For a homogeneous ideal, this number is independent of the monomial ordering used
-- (even though the elements of the monomial basis themselves are dependent on the ordering).
--
-- If the ideal I is not homogeneous, then R/I is not graded, and the Hilbert function is not well-defined.
-- Specifically, the number of degree i monomials in a basis is likely to depend on which monomial ordering you use.
hilbertFunQA :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
    [Vect k m] -> [Vect k m] -> Int -> Integer
hilbertFunQA vs gs i = hilbertFunQA' (ltIdeal gs) i
    where n = length vs
          hilbertFunQA' _ i | i < 0 = 0
          hilbertFunQA' (m:ms) i = hilbertFunQA' ms i - hilbertFunQA' (ms `quotientP` m) (i - deg m)
          hilbertFunQA' [] i = numMonomials n i
-- For example, consider k[x,y]/<x-y^2>
-- Under Lex ordering, the monomial basis is 1,y,y^2,y^3,...
-- Under Glex ordering, the monomial basis is 1,x,y,x^2,xy,x^3,x^2y,...
-- So the Hilbert function is not well-defined.
-- Note though that this function does still correctly return the number of degree i monomials for the given monomial ordering

-- naive implementation which simply counts monomials
hilbertSeriesQA1 vs gs = hilbertSeriesQA1' [1]
    where hilbertSeriesQA1' [] = [] -- repeat 0
          hilbertSeriesQA1' ms = length ms : hilbertSeriesQA1' (toSet [f | v <- vs, m <- ms, let f = v*m, f %% gs == f])

-- Eisenbud p325, p357 / Schenck p56
-- This can be made more efficient by choosing which m to recurse on
-- |Given variables vs, and a homogeneous ideal gs, @hilbertSeriesQA vs gs@ returns the Hilbert series for the quotient algebra k[vs]/\<gs\>.
-- The Hilbert series should be interpreted as a formal power series where the coefficient of t^i is the Hilbert function evaluated at i.
-- That is, the i'th element in the series is the number of degree i monomials in a basis for k[vs]/\<gs\>.
hilbertSeriesQA :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
    [Vect k m] -> [Vect k m] -> [Integer]
hilbertSeriesQA vs gs = hilbertSeriesQA' $ ltIdeal gs
    where hilbertSeriesQA' (m:ms) = hilbertSeriesQA' ms <-> (replicate (deg m) 0 ++ hilbertSeriesQA' (ms `quotientI` [m]))
          hilbertSeriesQA' [] = [numMonomials n i | i <- [0..] ]
          n = length vs
          (a:as) <-> (b:bs) = (a-b) : (as <-> bs)
          as <-> [] = as
          [] <-> bs = map negate bs

-- |In the case where every variable v occurs in some generator g of the homogeneous ideal (the usual case),
-- then the vs can be inferred from the gs.
-- @hilbertSeriesQA' gs@ returns the Hilbert series for the quotient algebra k[vs]/\<gs\>.
hilbertSeriesQA' :: (Fractional k, Ord k, MonomialConstructor m, Ord (m v), Monomial (m v), Algebra k (m v)) =>
    [Vect k (m v)] -> [Integer]
hilbertSeriesQA' gs = hilbertSeriesQA vs gs where vs = toSet (concatMap vars gs)

-- |For i \>\> 0, the Hilbert function becomes a polynomial in i, called the Hilbert polynomial.
hilbertPolyQA :: (Fractional k, Ord k, Monomial m, Ord m, Algebra k m) =>
     [Vect k m] -> [Vect k m] -> GlexPoly Q String
hilbertPolyQA vs gs = hilbertPolyQA' (ltIdeal gs) i
    where n = toInteger $ length vs
          i = glexvar "i"
          hilbertPolyQA' [] x = product [ x + fromInteger j | j <- [1..n-1] ] / (fromInteger $ product [1..n-1])
          hilbertPolyQA' (m:ms) x = hilbertPolyQA' ms x - hilbertPolyQA' (ms `quotientP` m) (x - fromIntegral (deg m))

hilbertPolyQA' :: (Fractional k, Ord k, MonomialConstructor m, Ord (m v), Monomial (m v), Algebra k (m v)) =>
    [Vect k (m v)] -> GlexPoly Q String
hilbertPolyQA' gs = hilbertPolyQA vs gs where vs = toSet (concatMap vars gs)

-- The dimension of a variety
dim vs gs = 1 + deg (hilbertPolyQA vs gs)

dim' gs = 1 + deg (hilbertPolyQA' gs)
