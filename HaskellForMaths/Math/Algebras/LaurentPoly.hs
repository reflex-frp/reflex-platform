-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}


module Math.Algebras.LaurentPoly where


import Math.Algebra.Field.Base hiding (powers)

import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures
import qualified Data.List as L

import Math.Algebras.Commutative -- for DivisionBasis and quotRemMP


-- LAURENT MONOMIALS

data LaurentMonomial = LM Int [(String,Int)] deriving (Eq,Ord)
{-
instance Ord LaurentMonomial where
    compare (LM si xis) (LM sj yjs) = compare (-si, xis) (-sj, yjs)
-}
instance Show LaurentMonomial where
    show (LM 0 []) = "1"
    show (LM _ xis) = concatMap (\(x,i) -> if i==1 then x else x ++ "^" ++ show i) xis

instance Mon LaurentMonomial where
    munit = LM 0 []
    mmult (LM si xis) (LM sj yjs) = LM (si+sj) $ addmerge xis yjs

instance (Eq k, Num k) => Algebra k LaurentMonomial where
    unit 0 = zero -- V []
    unit x = V [(munit,x)] 
    mult (V ts) = nf $ fmap (\(a,b) -> a `mmult` b) (V ts)
    -- mult (V ts) = nf $ V [(a `mmult` b, x) | (T a b, x) <- ts]

{-
-- This is just the Set Coalgebra, so better to use a generic instance
-- Also, not used anywhere. Hence commented out
instance Num k => Coalgebra k LaurentMonomial where
    counit (V ts) = sum [x | (m,x) <- ts] -- trace
    comult = fmap (\m -> T m m)
-}

type LaurentPoly k = Vect k LaurentMonomial

lvar v = V [(LM 1 [(v,1)], 1)] :: LaurentPoly Q

instance (Eq k, Fractional k) => Fractional (LaurentPoly k) where
    recip (V [(LM si xis,c)]) = V [(LM (-si) $ map (\(x,i)->(x,-i)) xis, recip c)]
    recip _ = error "LaurentPoly.recip: only defined for single terms"

q = lvar "q"
q' = 1/q


{-
-- division doesn't terminate with the derived Ord instance
-- if we use the graded Ord instance instead, division doesn't continue into negative powers
-- so we get the negative powers as remained, even if they're divisible
instance DivisionBasis LaurentMonomial where
    dividesB (LM si xis) (LM sj yjs) = si <= sj && dividesB' xis yjs where
        dividesB' ((x,i):xis) ((y,j):yjs) =
            case compare x y of
            LT -> False
            GT -> dividesB' ((x,i):xis) yjs
            EQ -> if i<=j then dividesB' xis yjs else False
        dividesB' [] _ = True
        dividesB' _ [] = False
    divB (LM si xis) (LM sj yjs) = LM (si-sj) $ divB' xis yjs where
        divB' ((x,i):xis) ((y,j):yjs) =
            case compare x y of
            LT -> (x,i) : divB' xis ((y,j):yjs)
            EQ -> if i == j then divB' xis yjs else (x,i-j) : divB' xis yjs -- we don't bother to check i > j
            GT -> error "divB'" -- (y,-j) : divB' ((x,i):xis) yjs
        divB' xis [] = xis
        divB' [] yjs = error "divB'"
-}
