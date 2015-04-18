-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE  MultiParamTypeClasses, FlexibleInstances #-}


module Math.Algebras.Matrix where

import Math.Algebra.Field.Base
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures


-- Mat2

{-
-- defined in Math.Algebras.TensorProduct
delta i j | i == j    = 1
          | otherwise = 0
-}

data Mat2 = E2 Int Int deriving (Eq,Ord,Show)
-- E i j represents the elementary matrix with a 1 at the (i,j) position, and 0s elsewhere

instance (Eq k, Num k) => Algebra k Mat2 where
    unit x = x *> V [(E2 i i, 1) | i <- [1..2] ]
    mult = linear mult' where
        mult' (E2 i j, E2 k l) = delta j k *> return (E2 i l)

-- In other words
-- unit x = x (1 0)
--            (0 1)
-- mult (a1 b1) `te` (a2 b2) = (a1 b1) * (a2 b2) = (a b)
--      (c1 d1)      (c2 d2)   (c1 d1)   (c2 d2)   (c d)

instance (Eq k, Num k) => Module k Mat2 EBasis where
    -- action ax = nf $ ax >>= action' where
    action = linear action' where
        action' (E2 i j, E k) = delta j k `smultL` return (E i)

-- In other words
-- action (a b) `te` (x) = (ax+by)
--        (c d)      (y)   (cx+dy)

toMat2 [[a,b],[c,d]] = sum $ zipWith (\x e -> unit x * return e) [a,b,c,d] [E2 1 1, E2 1 2, E2 2 1, E2 2 2]
-- fromMat2

toEB2 [x,y] = foldl add zero $ zipWith (\x e -> x `smultL` return e) [x,y] [E 1, E 2]

toEB xs = foldl add zero $ zipWith (\x e -> x `smultL` return e) xs (map E [1..])



data Mat2' = E2' Int Int deriving (Eq,Ord,Show)
-- E2' i j represents the dual basis element corresponding to E i j

-- Kassel p42
instance (Eq k, Num k) => Coalgebra k Mat2' where
    counit (V ts) = sum [xij * delta i j | (E2' i j, xij) <- ts]
    -- comult (V ts) = V $ concatMap (\(E2' i j,xij) -> [(T (E2' i k) (E2' k j), xij) | k <- [1..2]]) ts
    comult = linear (\(E2' i j) -> foldl (<+>) zero [return (E2' i k, E2' k j) | k <- [1..2]])
-- In other words
-- counit (a b) = (1 0)
--        (c d)   (0 1)
-- comult (a b) = (a1 b1) `te` (a2 b2)
--        (c d)   (c1 d1)      (c2 d2)
-- ??
-- ?? How does this act on Mat2?
-- ?? What is the relationship between this and SL2 ABCD, which it seems to resemble



data M3 = E3 Int Int deriving (Eq,Ord,Show)
-- E i j represents the elementary matrix with a 1 at the (i,j) position, and 0s elsewhere

instance (Eq k, Num k) => Algebra k M3 where
    unit 0 = zero -- V []
    unit x = V [(E3 i i, x) | i <- [1..3] ]
    -- mult (V ts) = nf $ V $ map (\((E3 i j, E3 k l), x) -> (E3 i l, delta j k * x)) ts
    mult = linear mult' where
        mult' (E3 i j, E3 k l) = delta j k *> return (E3 i l)

{-
-- Kassel p42
-- In this coalgebra instance, the E3 i j are to be interpreted as the dual basis, not the original basis
instance Num k => Coalgebra k M3 where
    counit (V ts) = sum [xij * delta i j | (E3 i j, xij) <- ts]
    comult (V ts) = V $ concatMap (\(E3 i j,xij) -> [((E3 i k, E3 k j), xij) | k <- [1..3]]) ts
-- (is this order preserving?)
-}
