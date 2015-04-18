-- Copyright (c) David Amos, 2008. All rights reserved.

module Math.Projects.KnotTheory.LaurentMPoly where

import qualified Data.Map as M
import Data.List as L

import Math.Algebra.Field.Base


-- It would be possible to refactor this and other code into common code for semigroup rings
-- But there are enough small fiddly differences that it's easier not to


-- LAURENT MONOMIALS

newtype LaurentMonomial = LM (M.Map String Q) deriving (Eq)
-- We allow exponents to be rationals, because we want to support rings such as Z[q^1/2,q^-1/2] in connection with Hecke algebras

-- the multidegree - not sure how meaningful this is if we have negative indices too
degLM (LM m) = sum $ M.elems m

-- Glex ordering
instance Ord LaurentMonomial where
    compare a b = let ds = M.elems m where LM m = a/b in
                  case compare (sum ds) 0 of
                  GT -> GT
                  LT -> LT
                  EQ -> if null ds then EQ else
                        if head ds > 0 then GT else LT

instance Show LaurentMonomial where
    show (LM a) | M.null a = "1"
                | otherwise = concatMap showVar $ M.toList a
                where showVar (v,1) = v
                      showVar (v,i) = v ++ "^" ++ show i

instance Num LaurentMonomial where
    LM a * LM b = LM $ M.filter (/=0) $ M.unionWith (+) a b
    fromInteger 1 = LM M.empty

instance Fractional (LaurentMonomial) where
    recip (LM m) = LM $ M.map negate m


-- untested
-- numeratorLM (LM a) = LM $ M.filter (>0) a

denominatorLM (LM a) = recip $ LM $ M.filter (<0) a

-- not valid for arguments with negative exponents (because 0 won't trump -i)
lcmLM (LM a) (LM b) = LM $ M.unionWith max a b

-- not tested -- for arguments with non-zero denominators
-- gcdLM (LM a) (LM b) = LM $ M.intersectionWith min a b

divLM a b = let LM c = a/b in if all (>=0) (M.elems c) then Just (LM c) else Nothing


-- LAURENT POLYNOMIALS

newtype LaurentMPoly r = LP [(LaurentMonomial,r)] deriving (Eq,Ord)

instance Show r => Show (LaurentMPoly r) where
    show (LP []) = "0"
    show (LP ts) =
        let (c:cs) = concatMap showTerm (reverse ts) -- we show Laurent polys with smallest terms first
        in if c == '+' then cs else c:cs
        where showTerm (m,c) =
                  case show c of
                  "1" -> "+" ++ show m
                  "-1" -> "-" ++ show m
                  -- cs@(x:_) -> (if x == '-' then cs else '+':cs) ++ (if m == 1 then "" else show m)
                  cs -> showCoeff cs ++ (if m == 1 then "" else show m)
              showCoeff (c:cs) = if any (`elem` ['+','-']) cs
                                 then "+(" ++ c:cs ++ ")"
                                 else if c == '-' then c:cs else '+':c:cs
              -- we don't attempt sign reversal within brackets in case we have expressions like t^-1 inside the brackets

instance (Eq r, Num r) => Num (LaurentMPoly r) where
    LP ts + LP us = LP (mergeTerms ts us)
    negate (LP ts) = LP $ map (\(m,c)->(m,-c)) ts
    LP ts * LP us = LP $ collect $ sortBy cmpTerm $ [(g*h,c*d) | (g,c) <- ts, (h,d) <- us]
    fromInteger 0 = LP []
    fromInteger n = LP [(fromInteger 1, fromInteger n)]

cmpTerm (a,c) (b,d) = case compare a b of EQ -> EQ; GT -> LT; LT -> GT
-- we have to put largest terms first so that quotRem works

-- inputs in descending order
mergeTerms (t@(g,c):ts) (u@(h,d):us) =
    case cmpTerm t u of
    LT -> t : mergeTerms ts (u:us)
    GT -> u : mergeTerms (t:ts) us
    EQ -> if e == 0 then mergeTerms ts us else (g,e) : mergeTerms ts us
    where e = c + d
mergeTerms ts us = ts ++ us -- one of them is null

collect (t1@(g,c):t2@(h,d):ts)
    | g == h = collect $ (g,c+d):ts
    | c == 0  = collect $ t2:ts
    | otherwise = t1 : collect (t2:ts)
collect ts = ts

-- Fractional instance so that we can enter fractional coefficients
-- Only lets us divide by single terms, not any other polynomials
instance (Eq r, Fractional r) => Fractional (LaurentMPoly r) where
    recip (LP [(m,c)]) = LP [(recip m, recip c)]
    recip _ = error "LaurentMPoly.recip: only supported for (non-zero) constants or monomials"


lm (LP ((m,c):ts)) = m
lc (LP ((m,c):ts)) = c
lt (LP ((m,c):ts)) = LP [(m,c)]

quotRemLP f g
    | g == 0 = error "quotRemLP: division by zero"
    | denominatorLP f /= 1 || denominatorLP g /= 1 = error "quotRemLP: negative exponents"
    | otherwise = quotRemLP' f (0,0)
    where
    quotRemLP' 0 (q,r) = (q,r)
    quotRemLP' h (q,r) =
        case lm h `divLM` lm g of
        Just m -> let t = LP [(m, lc h / lc g)]
                  in quotRemLP' (h-t*g) (q+t,r)
        Nothing -> let lth = lt h -- can't reduce lt h, so add it to the remainder and try to reduce the remaining terms
                   in quotRemLP' (h-lth) (q, r+lth)


-- g must be a binomial without negative exponents - eg i^2+1
reduceLP f g@(LP [_,_]) =
    let fn = f * fd
        fd = denominatorLP f
        (_,rn) = quotRemLP fn g
        (_,rd) = quotRemLP fd g
    in rn / rd


var v = LP [(LM $ M.singleton v 1, 1)]

t = var "t" :: LaurentMPoly Q
x = var "x" :: LaurentMPoly Q
y = var "y" :: LaurentMPoly Q
z = var "z" :: LaurentMPoly Q


denominatorLP (LP ts) = LP [(m',1)] where
    m' = foldl lcmLM 1 [denominatorLM m | (m,c) <- ts]

{-
-- not tested for terms with non-zero denominator
gcdTermsLP (LP ts) = LP [(m',1)] where
    m' = foldl gcdLM 1 [m | (m,c) <- ts]
-}

-- injection of field elements into polynomial ring
inject 0 = LP []
inject c = LP [(fromInteger 1, c)]

sqrtvar v = LP [(LM $ M.singleton v (1/2), 1)]

-- substitute terms for variables in an MPoly
-- eg subst [(x,a),(y,a+b),(z,c^2)] (x*y+z) -> a*(a+b)+c^2
subst vts (LP us) = sum [inject c * substM m | (m,c) <- us] where
    substM (LM m) = product [substV v ^^^ i | (v,i) <- M.toList m]
    substV v =
        let v' = var v in
        case L.lookup v' vts of
        Just t -> t
        Nothing -> v' -- no substitute, so keep as is

f ^^^ i | denominatorQ i == 1 = f ^^ numeratorQ i -- exponent is an integer
        | otherwise = case f of
                      LP [(LM m,1)] -> LP [(LM $ M.map (*i) m ,1)]-- base is a monomial
                      otherwise     -> error ("(^^^): Cannot calculate " ++ show f ++ " ^^^ " ++ show i)
{-
-- halve all indices - useful when we really want to be working over k[t^1/2,t^-1/2]
halfExponents (LP ts) =
    if any odd (concatMap (\(LM m,c) -> M.elems m) ts)
    then error ("halfExponents: " ++ show (LP ts))
    else LP $ map (\(LM m, c) -> (LM $ M.filter (/=0) $ M.map (`div` 2) m, c)) ts

halfExponents' f@(LP ts) =
    let f'@(LP us) = LP $ map (\(LM m, c) -> (LM $ M.map (`div` 2) m, c)) ts
    in if any (==0) (concatMap (\(LM m,c) -> M.elems m) us)
       then Left f
       else Right f'

quarterExponents' f@(LP ts) =
    let f'@(LP us) = LP $ map (\(LM m, c) -> (LM $ M.map (`div` 4) m, c)) ts
    in if any (==0) (concatMap (\(LM m,c) -> M.elems m) us)
       then Left f
       else Right f'
-}