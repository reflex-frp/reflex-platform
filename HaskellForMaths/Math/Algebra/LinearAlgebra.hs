-- Copyright (c) 2008-2012, David Amos. All rights reserved.

-- |A module providing elementary operations involving scalars, vectors, and matrices
-- over a ring or field. Vectors are represented as [a], matrices as [[a]].
-- (No distinction is made between row and column vectors.)
-- It is the caller's responsibility to ensure that the lists have the correct number of elements.
--
-- The mnemonic for many of the arithmetic operations is that the number of angle brackets
-- on each side indicates the dimension of the argument on that side. For example,
-- v \<*\>\> m is multiplication of a vector on the left by a matrix on the right.
module Math.Algebra.LinearAlgebra where

import qualified Data.List as L
import Math.Core.Field -- not actually used in this module


infixr 8 *>, *>>
infixr 7 <<*>
infixl 7 <.>, <*>, <<*>>, <*>>
infixl 6 <+>, <->, <<+>>, <<->>

-- The mnemonic for these operations is that the number of angle brackets on each side indicates the dimension of the argument on that side


-- vector operations

-- |u \<+\> v returns the sum u+v of vectors
(<+>) :: (Num a) => [a] -> [a] -> [a]
u <+> v = zipWith (+) u v

-- |u \<-\> v returns the difference u-v of vectors
(<->) :: (Num a) => [a] -> [a] -> [a]
u <-> v = zipWith (-) u v

-- |k *\> v returns the product k*v of the scalar k and the vector v
(*>) :: (Num a) => a -> [a] -> [a]
k *> v = map (k*) v

-- |u \<.\> v returns the dot product of vectors (also called inner or scalar product)
(<.>) :: (Num a) => [a] -> [a] -> a
u <.> v = sum (zipWith (*) u v)

-- |u \<*\> v returns the tensor product of vectors (also called outer or matrix product)
(<*>) :: (Num a) => [a] -> [a] -> [[a]]
u <*> v = [ [a*b | b <- v] | a <- u]


-- matrix operations

-- |a \<\<+\>\> b returns the sum a+b of matrices
(<<+>>) :: (Num a) => [[a]] -> [[a]] -> [[a]]
a <<+>> b = (zipWith . zipWith) (+) a b

-- |a \<\<-\>\> b returns the difference a-b of matrices
(<<->>) :: (Num a) => [[a]] -> [[a]] -> [[a]]
a <<->> b = (zipWith . zipWith) (-) a b

-- |a \<\<*\>\> b returns the product a*b of matrices
(<<*>>) :: (Num a) => [[a]] -> [[a]] -> [[a]]
a <<*>> b = [ [u <.> v | v <- L.transpose b] | u <- a]
 
-- |k *\>\> m returns the product k*m of the scalar k and the matrix m
(*>>) :: (Num a) => a -> [[a]] -> [[a]]
k *>> m = (map . map) (k*) m

-- |m \<\<*\> v is multiplication of a vector by a matrix on the left
(<<*>) :: (Num a) => [[a]] -> [a] -> [a]
m <<*> v = map (<.> v) m

-- |v \<*\>\> m is multiplication of a vector by a matrix on the right
(<*>>) :: (Num a) => [a] -> [[a]] -> [a]
v <*>> m = map (v <.>) (L.transpose m)


fMatrix n f = [[f i j | j <- [1..n]] | i <- [1..n]] 

-- version with indices from zero
fMatrix' n f = [[f i j | j <- [0..n-1]] | i <- [0..n-1]] 


-- idMx n = fMatrix n (\i j -> if i == j then 1 else 0)

idMx n = idMxs !! n where
    idMxs = map snd $ iterate next (0,[])
    next (j,m) = (j+1, (1 : replicate j 0) : map (0:) m)

-- |iMx n is the n*n identity matrix
iMx :: (Num t) => Int -> [[t]]
iMx n = idMx n

-- |jMx n is the n*n matrix of all 1s
jMx :: (Num t) => Int -> [[t]]
jMx n = replicate n (replicate n 1)

-- |zMx n is the n*n matrix of all 0s
zMx :: (Num t) => Int -> [[t]]
zMx n = replicate n (replicate n 0)

{-
-- VECTORS

data Vector d k = V [k] deriving (Eq,Ord,Show) 

instance (IntegerAsType d, Num k) => Num (Vector d k) where
    V a + V b = V $ a <+> b
    V a - V b = V $ a <-> b
    negate (V a) = V $ map negate a
    fromInteger 0 = V $ replicate d' 0 where d' = fromInteger $ value (undefined :: d)

V v <>> M m = V $ v <*>> m

M m <<> V v = V $ m <<*> v

k |> V v = V $ k *> v
-}

-- MATRICES

{-
-- Square matrices of dimension d over field k
data Matrix d k = M [[k]] deriving (Eq,Ord,Show)

instance (IntegerAsType d, Num k) => Num (Matrix d k) where
    M a + M b = M $ a <<+>> b
    M a - M b = M $ a <<->> b
    negate (M a) = M $ (map . map) negate a
    M a * M b = M $ a <<*>> b
    fromInteger 0 = M $ zMx d' where d' = fromInteger $ value (undefined :: d)
    fromInteger 1 = M $ idMx d' where d' = fromInteger $ value (undefined :: d)

instance (IntegerAsType d, Fractional a) => Fractional (Matrix d a) where
	recip (M a) = case inverse a of
		Nothing -> error "Matrix.recip: matrix is singular"
		Just a' -> M a'
-}


-- |The inverse of a matrix (over a field), if it exists
inverse :: (Eq a, Fractional a) => [[a]] -> Maybe [[a]]
inverse m =
    let d = length m -- the dimension
        i = idMx d
        m' = zipWith (++) m i
        i1 = inverse1 m'
        i2 = inverse2 i1
    in if length i1 == d
       then Just i2
       else Nothing

-- given (M|I), use row operations to get to (U|A), where U is upper triangular with 1s on diagonal
inverse1 [] = []
inverse1 ((x:xs):rs) =
    if x /= 0
    then let r' = (1/x) *> xs
         in (1:r') : inverse1 [ys <-> y *> r' | (y:ys) <- rs]
    else case filter (\r' -> head r' /= 0) rs of
         [] -> [] -- early termination, which will be detected in calling function
         r:_ -> inverse1 (((x:xs) <+> r) : rs)
-- This is basically row echelon form

-- given (U|A), use row operations to get to M^-1
inverse2 [] = []
inverse2 ((1:r):rs) = inverse2' r rs : inverse2 rs where
    inverse2' xs [] = xs
    inverse2' (x:xs) ((1:r):rs) = inverse2' (xs <-> x *> r) rs

xs ! i = xs !! (i-1) -- ie, a 1-based list lookup instead of 0-based

rowEchelonForm [] = []
rowEchelonForm ((x:xs):rs) =
    if x /= 0
    then let r' = (1/x) *> xs
         in (1:r') : map (0:) (rowEchelonForm [ys <-> y *> r' | (y:ys) <- rs])
    else case filter (\r' -> head r' /= 0) rs of
         [] -> map (0:) (rowEchelonForm $ xs : map tail rs)
         r:_ -> rowEchelonForm (((x:xs) <+> r) : rs)
rowEchelonForm zs@([]:_) = zs

reducedRowEchelonForm :: (Eq a, Fractional a) => [[a]] -> [[a]]
reducedRowEchelonForm m = reverse $ reduce $ reverse $ rowEchelonForm m where
    reduce (r:rs) = let r':rs' = reduceStep (r:rs) in r' : reduce rs' -- is this scanl or similar?
    reduce [] = []
    reduceStep ((1:xs):rs) = (1:xs) : [ 0: (ys <-> y *> xs) | y:ys <- rs]
    reduceStep rs@((0:_):_) = zipWith (:) (map head rs) (reduceStep $ map tail rs)
    reduceStep rs = rs

-- Given a matrix m and (column) vector b, either find (column vector) x such that m x == b,
-- or indicate that there is none
solveLinearSystem m b =
    let augmented = zipWith (\r x -> r ++ [x]) m b -- augmented matrix
        trisystem = inverse1 augmented -- upper triangular form
        solution = reverse $ solveTriSystem $ reverse $ map reverse trisystem
    in if length solution == length b then Just solution else Nothing
    where solveTriSystem ([v,c]:rs) =
              let x = v/c -- the first row tells us that cx == v
                  rs' = map (\(v':c':r) -> (v'-c'*x):r) rs
              in x : solveTriSystem rs'
          solveTriSystem [] = []
          solveTriSystem _ = [] -- abnormal termination - m wasn't invertible


isZero v = all (==0) v

-- inSpanRE m v returns whether the vector v is in the span of the matrix m, where m is required to be in row echelon form
inSpanRE ((1:xs):bs) (y:ys) = inSpanRE (map tail bs) (if y == 0 then ys else ys <-> y *> xs)
inSpanRE ((0:xs):bs) (y:ys) = if y == 0 then inSpanRE (xs : map tail bs) ys else False
inSpanRE _ ys = isZero ys

rank m = length $ filter (not . isZero) $ rowEchelonForm m

-- kernel of a matrix
-- returns basis for vectors v s.t m <<*> v == 0
kernel m = kernelRRE $ reducedRowEchelonForm m

kernelRRE m =
    let nc = length $ head m -- the number of columns
        is = findLeadingCols 1 (L.transpose m) -- these are the indices of the columns which have a leading 1
        js = [1..nc] L.\\ is
        freeCols = let m' = take (length is) m -- discard zero rows
                   in zip is $ L.transpose [map (negate . (!j)) m' | j <- js]
        boundCols = zip js (idMx $ length js)
    in L.transpose $ map snd $ L.sort $ freeCols ++ boundCols
    where
    findLeadingCols i (c@(1:_):cs) = i : findLeadingCols (i+1) (map tail cs)
    findLeadingCols i (c@(0:_):cs) = findLeadingCols (i+1) cs
    findLeadingCols _ _ = []

-- m ^- n = recip m ^ n

-- t (M m) = M (L.transpose m)

-- |The determinant of a matrix (over a field)
det :: (Eq a, Fractional a) => [[a]] -> a
det [[x]] = x
det ((x:xs):rs) =
    if x /= 0
    then let r' = (1/x) *> xs
         in x * det [ys <-> y *> r' | (y:ys) <- rs]
    else case filter (\r' -> head r' /= 0) rs of
         [] -> 0
         r:_ -> det (((x:xs) <+> r) : rs)


{-
class IntegerAsType a where
    value :: a -> Integer

data Z
instance IntegerAsType Z where
    value _ = 0

data S a
instance IntegerAsType a => IntegerAsType (S a) where
    value _ = value (undefined :: a) + 1
-}