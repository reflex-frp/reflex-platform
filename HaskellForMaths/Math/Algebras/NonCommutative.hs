-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |A module defining the algebra of non-commutative polynomials over a field k
module Math.Algebras.NonCommutative where

import Math.Algebra.Field.Base hiding (powers)
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures
import qualified Data.List as L


data NonComMonomial v = NCM Int [v] deriving (Eq)

instance Ord v => Ord (NonComMonomial v) where
    compare (NCM lx xs) (NCM ly ys) = compare (-lx, xs) (-ly, ys)
-- ie Glex ordering

instance (Eq v, Show v) => Show (NonComMonomial v) where
    show (NCM _ []) = "1"
    show (NCM _ vs) = concatMap showPower (L.group vs)
        where showPower [v] = showVar v
              showPower vs@(v:_) = showVar v ++ "^" ++ show (length vs)
              showVar v = filter (/= '"') (show v)

instance Mon (NonComMonomial v) where
    munit = NCM 0 []
    mmult (NCM i xs) (NCM j ys) = NCM (i+j) (xs++ys)

instance (Eq k, Num k, Ord v) => Algebra k (NonComMonomial v) where
    unit 0 = zero -- V []
    unit x = V [(munit,x)]
    mult = nf . fmap (\(a,b) -> a `mmult` b)

{-
-- This is the monoid algebra for non-commutative monomials (which is the free monoid)
instance (Num k, Ord v) => Algebra k (NonComMonomial v) where
    unit 0 = zero -- V []
    unit x = V [(munit,x)] where munit = NCM 0 []
    mult (V ts) = nf $ fmap (\(a,b) -> a `mmult` b) (V ts)
        where mmult (NCM lu us) (NCM lv vs) = NCM (lu+lv) (us++vs)
    -- mult (V ts) = nf $ V [(a `mmult` b, x) | (T a b, x) <- ts]
-}

{-
-- This is just the Set Coalgebra, so better to use a generic instance
-- Also, not used anywhere. Hence commented out
instance Num k => Coalgebra k (NonComMonomial v) where
    counit (V ts) = sum [x | (m,x) <- ts] -- trace
    comult = fmap (\m -> (m,m))
-}


class Monomial m where
    var :: v -> Vect Q (m v)
    powers :: Eq v => m v -> [(v,Int)]
-- why do we need "powers"??

V ts `bind` f = sum [c *> product [f x ^ i | (x,i) <- powers m] | (m, c) <- ts] 
-- flipbind f = linear (\m -> product [f x ^ i | (x,i) <- powers m])

instance Monomial NonComMonomial where
    var v = V [(NCM 1 [v],1)]
    powers (NCM _ vs) = map power (L.group vs)
        where power vs@(v:_) = (v,length vs)


type NCPoly v = Vect Q (NonComMonomial v)

{-
x,y,z :: NCPoly String
x = var "x"
y = var "y"
z = var "z"
-}


-- DIVISION

class DivisionBasis m where
    divM :: m -> m -> Maybe (m,m)
    -- divM a b tries to find l, r such that a = lbr
{-
    findOverlap :: m -> m -> Maybe (m,m,m)
    -- given two monomials f g, find if possible a,b,c with f=ab g=bc
-}

instance Eq v => DivisionBasis (NonComMonomial v) where
    divM (NCM _ a) (NCM _ b) = divM' [] a where
        divM' ls (r:rs) =
            if b `L.isPrefixOf` (r:rs)
            then Just (ncm $ reverse ls, ncm $ drop (length b) (r:rs))
            else divM' (r:ls) rs
        divM' _ [] = Nothing
{-
    findOverlap (NCM _ xs) (NCM _ ys) = findOverlap' [] xs ys where
        findOverlap' as [] cs = Nothing -- (reverse as, [], cs)
        findOverlap' as (b:bs) cs =
            if (b:bs) `L.isPrefixOf` cs
            then Just (ncm $ reverse as, ncm $ b:bs, ncm $ drop (length (b:bs)) cs)
            else findOverlap' (b:as) bs cs
-}
ncm xs = NCM (length xs) xs

lm (V ((m,c):ts)) = m
lc (V ((m,c):ts)) = c
lt (V (t:ts)) = V [t]

-- given f, gs, find ls, rs, f' such that f = sum (zipWith3 (*) ls gs rs) + f', with f' not divisible by any g
quotRemNP f gs | all (/=0) gs = quotRemNP' f (replicate n (0,0), 0)
               | otherwise = error "quotRemNP: division by zero"
    where
    n = length gs
    quotRemNP' 0 (lrs,f') = (lrs,f')
    quotRemNP' h (lrs,f') = divisionStep h (gs,[],lrs,f')
    divisionStep h (g:gs, lrs', (l,r):lrs, f') =
        case lm h `divM` lm g of
        Just (l',r') -> let l'' = V [(l',lc h / lc g)]
                            r'' = V [(r',1)]
                            h' = h - l'' * g * r''
                        in quotRemNP' h' (reverse lrs' ++ (l+l'',r+r''):lrs, f')
        Nothing -> divisionStep h (gs,(l,r):lrs',lrs,f')
    divisionStep h ([],lrs',[],f') =
        let lth = lt h -- can't reduce lt h, so add it to the remainder and try to reduce the remaining terms
        in quotRemNP' (h-lth) (reverse lrs', f'+lth)

-- It is only marginally (5-10%) more space/time efficient not to track the (lazily unevaluated) factors
remNP f gs | all (/=0) gs = remNP' f 0
           | otherwise = error "remNP: division by zero"
    where
    n = length gs
    remNP' 0 f' = f'
    remNP' h f' = divisionStep h gs f'
    divisionStep h (g:gs) f' =
        case lm h `divM` lm g of
        Just (l',r') -> let l'' = V [(l',lc h / lc g)]
                            r'' = V [(r',1)]
                            h' = h - l'' * g * r''
                        in remNP' h' f'
        Nothing -> divisionStep h gs f'
    divisionStep h [] f' =
        let lth = lt h -- can't reduce lt h, so add it to the remainder and try to reduce the remaining terms
        in remNP' (h-lth) (f'+lth)

infixl 7 %%
-- f %% gs = r where (_,r) = quotRemNP f gs
f %% gs = remNP f gs

