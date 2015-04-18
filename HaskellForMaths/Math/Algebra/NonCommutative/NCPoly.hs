-- Copyright (c) David Amos, 2008. All rights reserved.

-- |A module providing a type for non-commutative polynomials.
module Math.Algebra.NonCommutative.NCPoly where

import Data.List as L
import Math.Algebra.Field.Base


-- (NON-COMMUTATIVE) MONOMIALS

newtype Monomial v = M [v] deriving (Eq)

instance Ord v => Ord (Monomial v) where
    compare (M xs) (M ys) = compare (length xs,xs) (length ys,ys)
-- Glex ordering

instance (Eq v, Show v) => Show (Monomial v) where
    show (M xs) | null xs = "1"
                | otherwise = concatMap showPower (L.group xs)
        where showPower [v] = showVar v
              showPower vs@(v:_) = showVar v ++ "^" ++ show (length vs)
              showVar v = filter (/= '"') (show v)
-- Taken from NonComMonomial - why don't we just use it directly

instance (Eq v, Show v) => Num (Monomial v) where
    M xs * M ys = M (xs ++ ys)
    fromInteger 1 = M []

-- try to find l, r such that a = lbr
divM (M a) (M b) = divM' [] a where
    divM' ls (r:rs) =
        if b `L.isPrefixOf` (r:rs)
        then Just (M $ reverse ls, M $ drop (length b) (r:rs))
        else divM' (r:ls) rs
    divM' _ [] = Nothing


-- (NON-COMMUTATIVE) POLYNOMIALS

newtype NPoly r v = NP [(Monomial v,r)] deriving (Eq)

instance (Ord r, Ord v) => Ord (NPoly r v) where
    compare (NP ts) (NP us) = compare ts us

instance (Show r, Eq v, Show v) => Show (NPoly r v) where
    show (NP []) = "0"
    show (NP ts) =
        let (c:cs) = concatMap showTerm ts
        in if c == '+' then cs else c:cs
        where showTerm (m,a) =
                  case show a of
                  "1" -> "+" ++ show m
                  "-1" -> "-" ++ show m
                  -- cs@(x:_) -> (if x == '-' then cs else '+':cs) ++ (if m == 1 then "" else show m)
                  cs -> showCoeff cs ++ (if m == 1 then "" else show m)
              showCoeff (c:cs) = if any (`elem` ['+','-']) cs
                                 then "+(" ++ c:cs ++ ")"
                                 else if c == '-' then c:cs else '+':c:cs

instance (Eq r, Num r, Ord v, Show v) => Num (NPoly r v) where
    NP ts + NP us = NP (mergeTerms ts us)
    negate (NP ts) = NP $ map (\(m,c) -> (m,-c)) ts
    NP ts * NP us = NP $ collect $ L.sortBy cmpTerm $ [(g*h,c*d) | (g,c) <- ts, (h,d) <- us]
    fromInteger 0 = NP []
    fromInteger n = NP [(fromInteger 1, fromInteger n)]

cmpTerm (a,c) (b,d) = case compare a b of EQ -> EQ; GT -> LT; LT -> GT -- in mpolys we put "larger" terms first

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
-- Only lets us divide by field elements (with unit monomial), not any other polynomials
instance (Eq k, Fractional k, Ord v, Show v) => Fractional (NPoly k v) where
    recip (NP [(1,c)]) = NP [(1, recip c)]
    recip _ = error "NPoly.recip: only supported for (non-zero) constants"


-- SOME VARIABLES (INDETERMINATES)
-- The idea is that you define your own type of indeterminates as required, along the same lines as this

data Var = X | Y | Z deriving (Eq,Ord)

instance Show Var where
    show X = "x"
    show Y = "y"
    show Z = "z"

-- |Create a non-commutative variable for use in forming non-commutative polynomials.
-- For example, we could define x = var "x", y = var "y". Then x*y /= y*x.
var :: (Num k) => v -> NPoly k v
var v = NP [(M [v], 1)]

x = var X :: NPoly Q Var
y = var Y :: NPoly Q Var
z = var Z :: NPoly Q Var


-- DIVISION ALGORITHM

lm (NP ((m,c):ts)) = m
lc (NP ((m,c):ts)) = c
lt (NP (t:ts)) = NP [t]

-- given f, gs, find ls, rs, f' such that f = sum (zipWith3 (*) ls gs rs) + f', with f' not divisible by any g
quotRemNP f gs | all (/=0) gs = quotRemNP' f (replicate n (0,0), 0)
               | otherwise = error "quotRemNP: division by zero"
    where
    n = length gs
    quotRemNP' 0 (lrs,f') = (lrs,f')
    quotRemNP' h (lrs,f') = divisionStep h (gs,[],lrs,f')
    divisionStep h (g:gs, lrs', (l,r):lrs, f') =
        case lm h `divM` lm g of
        Just (l',r') -> let l'' = NP [(l',lc h / lc g)]
                            r'' = NP [(r',1)]
                            h' = h - l'' * g * r''
                        in quotRemNP' h' (reverse lrs' ++ (l+l'',r+r''):lrs, f')
        Nothing -> divisionStep h (gs,(l,r):lrs',lrs,f')
    divisionStep h ([],lrs',[],f') =
        let lth = lt h -- can't reduce lt h, so add it to the remainder and try to reduce the remaining terms
        in quotRemNP' (h-lth) (reverse lrs', f'+lth)

-- It is only marginally (5-10%) more space/time efficient not to track the (lazily unevaluated) factors
remNP f gs | all (/=0) gs = remNP' f 0
-- let result = remNP' f 0 in if result == remNP2 f gs then result else error ("remNP2 " ++ show f ++ " " ++ show gs)
           | otherwise = error "remNP: division by zero"
    where
    n = length gs
    remNP' 0 f' = f'
    remNP' h f' = divisionStep h gs f'
    divisionStep h (g:gs) f' =
        case lm h `divM` lm g of
        Just (l',r') -> let l'' = NP [(l',lc h / lc g)]
                            r'' = NP [(r',1)]
                            h' = h - l'' * g * r''
                        in remNP' h' f'
        Nothing -> divisionStep h gs f'
    divisionStep h [] f' =
        let lth = lt h -- can't reduce lt h, so add it to the remainder and try to reduce the remaining terms
        in remNP' (h-lth) (f'+lth)

infixl 7 %%
-- f %% gs = r where (_,r) = quotRemNP f gs
f %% gs = remNP f gs

-- !! Not sure if the following is valid
-- The idea is to avoid dividing by lc g, because sometimes our coefficient ring is not a field
-- Passes all the knot theory tests
-- However, it may be that if we ever get a non-invertible element at the front, we are in trouble anyway
remNP2 f gs | all (/=0) gs = remNP' f 0
           | otherwise = error "remNP: division by zero"
    where
    n = length gs
    remNP' 0 f' = f'
    remNP' h f' = divisionStep h gs f'
    divisionStep h (g:gs) f' =
        case lm h `divM` lm g of
        Just (l',r') -> let l'' = NP [(l',1)] -- NP [(l',lc h / lc g)]
                            r'' = NP [(r',1)]
                            lcg = inject (lc g)
                            lch = inject (lc h)
                            -- h' = h - l'' * g * r''
                            h' = lcg * h - lch * l'' * g * r''
                        in remNP' h' (lcg * f') -- must multiply f' by lcg too (otherwise get incorrect results, eg tlBasis 4)
        Nothing -> divisionStep h gs f'
    divisionStep h [] f' =
        let lth = lt h -- can't reduce lt h, so add it to the remainder and try to reduce the remaining terms
        in remNP' (h-lth) (f'+lth)


-- OTHER STUFF

toMonic 0 = 0
toMonic (NP ts@((_,c):_))
    | c == 1 = NP ts
    | otherwise = NP $ map (\(m,d)->(m,d/c)) ts

-- injection of field elements into polynomial ring
inject 0 = NP []
inject c = NP [(fromInteger 1, c)]

-- substitute terms for variables in an NPoly
-- eg subst [(x,a),(y,a+b),(z,c^2)] (x*y+z) -> a*(a+b)+c^2
subst vts (NP us) = sum [inject c * substM m | (m,c) <- us] where
    substM (M xs) = product [substV x | x <- xs]
    substV v =
        let v' = NP [(M [v], 1)] in
        case L.lookup v' vts of
        Just t -> t
        Nothing -> error ("subst: no substitute supplied for " ++ show v')


-- INVERTIBLE
-- To support algebras which have invertible elements

class Invertible a where
    inv :: a -> a

x ^- k = inv x ^ k