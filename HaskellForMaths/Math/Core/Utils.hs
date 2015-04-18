-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}

-- |A module of simple utility functions which are used throughout the rest of the library
module Math.Core.Utils where

import Data.List as L
import qualified Data.Set as S


toSet = S.toList . S.fromList

sortDesc = L.sortBy (flip compare)

insertDesc = L.insertBy (flip compare)


-- |The set union of two ascending lists. If both inputs are strictly increasing, then the output is their union
-- and is strictly increasing. The code does not check that the lists are strictly increasing.
setUnionAsc :: Ord a => [a] -> [a] -> [a]
setUnionAsc (x:xs) (y:ys) =
    case compare x y of
    LT -> x : setUnionAsc xs (y:ys)
    EQ -> x : setUnionAsc xs ys
    GT -> y : setUnionAsc (x:xs) ys
setUnionAsc xs ys = xs ++ ys

-- |The multiset sum of two ascending lists. If xs and ys are ascending, then multisetSumAsc xs ys == sort (xs++ys).
-- The code does not check that the lists are ascending.
multisetSumAsc :: Ord a => [a] -> [a] -> [a]
multisetSumAsc (x:xs) (y:ys) =
    case compare x y of
    LT -> x : multisetSumAsc xs (y:ys)
    EQ -> x : y : multisetSumAsc xs ys
    GT -> y : multisetSumAsc (x:xs) ys
multisetSumAsc xs ys = xs ++ ys

-- |The multiset sum of two descending lists. If xs and ys are descending, then multisetSumDesc xs ys == sort (xs++ys).
-- The code does not check that the lists are descending.
multisetSumDesc :: Ord a => [a] -> [a] -> [a]
multisetSumDesc (x:xs) (y:ys) =
    case compare x y of
    GT -> x : multisetSumDesc xs (y:ys)
    EQ -> x : y : multisetSumDesc xs ys
    LT -> y : multisetSumDesc (x:xs) ys
multisetSumDesc xs ys = xs ++ ys


-- |The multiset or set difference between two ascending lists. If xs and ys are ascending, then diffAsc xs ys == xs \\ ys,
-- and diffAsc is more efficient. If xs and ys are sets (that is, have no repetitions), then diffAsc xs ys is the set difference.
-- The code does not check that the lists are ascending.
diffAsc :: Ord a => [a] -> [a] -> [a]
diffAsc (x:xs) (y:ys) = case compare x y of
                        LT -> x : diffAsc xs (y:ys)
                        EQ -> diffAsc xs ys
                        GT -> diffAsc (x:xs) ys
diffAsc xs [] = xs
diffAsc [] _ = []

-- |The multiset or set difference between two descending lists. If xs and ys are descending, then diffDesc xs ys == xs \\ ys,
-- and diffDesc is more efficient. If xs and ys are sets (that is, have no repetitions), then diffDesc xs ys is the set difference.
-- The code does not check that the lists are descending.
diffDesc :: Ord a => [a] -> [a] -> [a]
diffDesc (x:xs) (y:ys) = case compare x y of
                        GT -> x : diffDesc xs (y:ys)
                        EQ -> diffDesc xs ys
                        LT -> diffDesc (x:xs) ys
diffDesc xs [] = xs
diffDesc [] _ = []


isSubsetAsc = isSubMultisetAsc

isSubMultisetAsc (x:xs) (y:ys) =
    case compare x y of
    LT -> False
    EQ -> isSubMultisetAsc xs ys
    GT -> isSubMultisetAsc (x:xs) ys
isSubMultisetAsc [] ys = True
isSubMultisetAsc xs [] = False


pairs (x:xs) = map (x,) xs ++ pairs xs
pairs [] = []

ordpair x y | x < y     = (x,y)
            | otherwise = (y,x)


-- fold a comparison operator through a list
foldcmpl p xs = and $ zipWith p xs (tail xs)
-- foldcmpl p (x1:x2:xs) = p x1 x2 && foldcmpl p (x2:xs)
-- foldcmpl _ _ = True

-- foldcmpl _ [] = True
-- foldcmpl p xs = and $ zipWith p xs (tail xs)

isWeaklyIncreasing :: Ord t => [t] -> Bool
isWeaklyIncreasing = foldcmpl (<=)

isStrictlyIncreasing :: Ord t => [t] -> Bool
isStrictlyIncreasing = foldcmpl (<)

isWeaklyDecreasing :: Ord t => [t] -> Bool
isWeaklyDecreasing = foldcmpl (>=)

isStrictlyDecreasing :: Ord t => [t] -> Bool
isStrictlyDecreasing = foldcmpl (>)

-- for use with L.sortBy
cmpfst x y = compare (fst x) (fst y)

-- for use with L.groupBy
eqfst x y = (==) (fst x) (fst y)


fromBase b xs = foldl' (\n x -> n * b + x) 0 xs

-- |Given a set @xs@, represented as an ordered list, @powersetdfs xs@ returns the list of all subsets of xs, in lex order
powersetdfs :: [a] -> [[a]]
powersetdfs xs = map reverse $ dfs [ ([],xs) ]
    where dfs ( (ls,rs) : nodes ) = ls : dfs (successors (ls,rs) ++ nodes)
          dfs [] = []
          successors (ls,rs) = [ (r:ls, rs') | r:rs' <- L.tails rs ]

-- |Given a set @xs@, represented as an ordered list, @powersetbfs xs@ returns the list of all subsets of xs, in shortlex order
powersetbfs :: [a] -> [[a]]
powersetbfs xs = map reverse $ bfs [ ([],xs) ]
    where bfs ( (ls,rs) : nodes ) = ls : bfs ( nodes ++ successors (ls,rs) )
          bfs [] = []
          successors (ls,rs) = [ (r:ls, rs') | r:rs' <- L.tails rs ]


-- |Given a positive integer @k@, and a set @xs@, represented as a list,
-- @combinationsOf k xs@ returns all k-element subsets of xs.
-- The result will be in lex order, relative to the order of the xs.
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) | k > 0 = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

-- |@choose n k@ is the number of ways of choosing k distinct elements from an n-set
choose :: (Integral a) => a -> a -> a
choose n k = product [n-k+1..n] `div` product [1..k]


-- |The class of finite sets
class FinSet x where
    elts :: [x]

-- |A class representing algebraic structures having an inverse operation.
-- Note that in some cases not every element has an inverse.
class HasInverses a where
    inverse :: a -> a

infix 8 ^-

-- |A trick: x^-1 returns the inverse of x
(^-) :: (Num a, HasInverses a, Integral b) => a -> b -> a
x ^- n = inverse x ^ n