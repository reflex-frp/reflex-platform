-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}


module Math.Combinatorics.Poset where

import Math.Common.ListSet as LS -- set operations on strictly ascending lists
import Math.Core.Utils -- for set/multiset operations on ordered lists
import Math.Algebra.Field.Base
import Math.Combinatorics.FiniteGeometry
import Math.Algebra.LinearAlgebra

import Math.Combinatorics.Digraph

import Data.List as L
import qualified Data.Map as M
-- import qualified Data.Set as S


-- |A poset is represented as a pair (set,po), where set is the underlying set of the poset, and po is the partial order relation
newtype Poset t = Poset ([t], t -> t -> Bool)

instance Eq t => Eq (Poset t) where
    Poset (set,po) == Poset (set',po') =
        set == set' && and [po x y == po' x y | x <- set, y <- set]
-- There may be ways to avoid comparing every pair
-- If we could calculate the coverGraph without comparing every pair,
-- then it would be sufficient to test whether their cover graphs are equal

instance Show t => Show (Poset t) where
    show (Poset (set,po)) = "Poset " ++ show set

implies p q = q || not p

isReflexive (set,po) = and [x `po` x | x <- set]
isAntisymmetric (set,po) = and [((x `po` y) && (y `po` x)) `implies` (x == y) | x <- set, y <- set]
isTransitive (set,po) = and [((x `po` y) && (y `po` z)) `implies` (x `po` z) | x <- set, y <- set, z <- set]

isPoset poset = isReflexive poset && isAntisymmetric poset && isTransitive poset
    
poset (set,po)
    | isPoset (set,po) = Poset (set,po)
    | otherwise = error "poset: Not a partial order"


-- Most of the posets we will deal with are in fact lattices, meaning that any two elements
-- have a meet (greatest lower bound) and join (least upper bound)

intervals (Poset (set,po)) = [(a,b) | a <- set, b <- set, a `po` b]

interval (Poset (set,po)) (x,z) = [y | y <- set, x `po` y, y `po` z]


-- LINEAR ORDER POSET
-- This is of course a lattice, with meet = min, join = max

-- |A chain is a poset in which every pair of elements is comparable (ie either x <= y or y <= x).
-- It is therefore a linear or total order.
-- chainN n is the poset consisting of the numbers [1..n] ordered by (<=)
chainN :: Int -> Poset Int
chainN n = Poset ( [1..n], (<=) )

-- hasseN n = DG [1..n] [(i,i+1) | i <- [1..n-1]]


-- |An antichain is a poset in which distinct elements are incomparable.
-- antichainN n is the poset consisting of [1..n], with x <= y only when x == y.
antichainN :: Int -> Poset Int
antichainN n = Poset ( [1..n], (==) )


-- LATTICE OF (POSITIVE) DIVISORS OF N

divides a b = b `mod` a == 0

divisors n | n >= 1 = [a | a <- [1..n], a `divides` n]

-- |posetD n is the lattice of (positive) divisors of n
posetD :: Int -> Poset Int
posetD n | n >= 1 = Poset ( divisors n, divides )


-- LATTICE OF SUBSETS OF [1..N] ORDERED BY INCLUSION
-- (Boolean lattice)

powerset [] = [[]]
powerset (x:xs) = let p = powerset xs in p ++ map (x:) p

{-
-- subset test for sorted lists
isSubset (x:xs) (y:ys) =
    case compare x y of
    LT -> False
    EQ -> isSubset xs ys
    GT -> isSubset (x:xs) ys
isSubset [] _ = True
isSubset _ [] = False
-}

-- |posetB n is the lattice of subsets of [1..n] ordered by inclusion
posetB :: Int -> Poset [Int]
posetB n = Poset ( powerset [1..n], LS.isSubset )


-- LATTICE OF SET PARTITIONS OF [1..N] ORDERED BY REFINEMENT

partitions [] = [[]]
partitions [x] = [[[x]]]
partitions (x:xs) = let ps = partitions xs in
    map ([x]:) ps ++ [ (x:cell):(L.delete cell p) | p <- ps, cell <- p]
-- if the input is sorted, then so is the output

isRefinement a b = and [or [acell `isSubset` bcell | bcell <- b] | acell <- a]
-- if we know that a and b are appropriately sorted, then this can probably be done more efficiently

-- |posetP n is the lattice of set partitions of [1..n], ordered by refinement
posetP :: Int -> Poset [[Int]]
posetP n = Poset ( partitions [1..n], isRefinement )

-- muP n = ...
-- see van Lint and Wilson p336


-- LATTICE OF INTERVAL PARTITIONS OF [1..N] ORDERED BY REFINEMENT

intervalPartitions xs = filter (all isInterval) (partitions xs)

isInterval (x1:x2:xs) = x1+1 == x2 && isInterval (x2:xs)
isInterval _ = True

intervalPartitions2 [] = [[]]
intervalPartitions2 [x] = [[[x]]]
intervalPartitions2 (x:xs) = let ips = intervalPartitions2 xs in
    map ([x]:) ips ++ [ (x:head):tail | (head:tail) <- ips]
-- we're guaranteed that x+1 is at the head of the head


-- LATTICE OF INTEGER PARTITIONS OF N ORDERED BY REFINEMENT

integerPartitions1 n = ips (reverse [1..n]) n
    where ips [] 0 = [[]]
          ips [] _ = []
          ips (x:xs) n | x > n     = ips xs n
                       | otherwise = map (x:) (ips (x:xs) (n-x)) ++ ips xs n

-- For example, integerPartitions 5 -> [ [5], [4,1], [3,2], [3,1,1], [2,2,1], [2,1,1,1], [1,1,1,1,1] ]
integerPartitions n = dfs ([],n,n)
    where dfs (xs, 0, _) = [reverse xs]
          dfs (xs, r, i) = concatMap dfs [ (i':xs, r-i', i') | i' <- reverse [1..min r i] ]

isIPRefinement ys xs = dfs xs ys
    where dfs (x:xs) (y:ys) | x < y = False
                            | x == y = dfs xs ys
                            | otherwise = or [dfs xs' ys' | y' <- y:ys, let ys' = L.delete y' (y:ys),
                                                                        let xs' = insertDesc (x-y') xs]
          dfs [] [] = True
          insertDesc = L.insertBy (flip compare)

{-
-- In theory it feels like this ought to be faster for large n, but in practice it's unclear
isIPRefinement2 ys xs = isIPRefinement (ys \\ xs) (xs \\ ys)
    where (\\) = diffDesc
-}

-- |posetIP n is the poset of integer partitions of n, ordered by refinement
posetIP :: Int -> Poset [Int]
posetIP n = Poset (integerPartitions n, isIPRefinement)


-- Could also implement the Young lattice (or the part up to n)
-- Of integer partitions <= n, ordered by inclusion
-- Kassel, Turaev, Braid Groups, p202


-- INTEGER COMPOSITIONS


-- LATTICE OF SUBSPACES OF Fq^n

subspaces fq n = [] : concatMap (flatsPG (n-1) fq) [0..n-1]
-- note that flatsPG returns the subspaces as a matrix of row vectors in reduced row echelon form

-- inSpanRE m v returns whether the vector v is in the span of the rows of the matrix m, where m is required to be in row echelon form
isSubspace s1 s2 = all (inSpanRE s2) s1

-- This is the projective geometry PG(n,q)
-- |posetL n fq is the lattice of subspaces of the vector space Fq^n, ordered by inclusion.
-- Subspaces are represented by their reduced row echelon form.
posetL :: (Eq fq, FiniteField fq) => Int -> [fq] -> Poset [[fq]]
posetL n fq = Poset ( subspaces fq n, isSubspace ) 


-- choose n k = product [n-k+1..n] `div` product [1..k]


-- NEW FROM OLD CONSTRUCTIONS


-- |The subposet of a poset satisfying a predicate
subposet :: Poset a -> (a -> Bool) -> Poset a
subposet (Poset (set,po)) p = Poset (filter p set, po)

-- |The direct sum of two posets
dsum :: Poset a -> Poset b -> Poset (Either a b)
dsum (Poset (setA,poA)) (Poset (setB,poB)) = Poset (set,po)
    where set = map Left setA ++ map Right setB
          po (Left a1) (Left a2) = poA a1 a2
          po (Right b1) (Right b2) = poB b1 b2
          po _ _ = False

-- |The direct product of two posets
dprod :: Poset a -> Poset b -> Poset (a,b)
dprod (Poset (setA,poA)) (Poset (setB,poB)) =
    Poset ( [(a,b) | a <- setA, b <- setB], \(a1,b1) (a2,b2) -> (a1 `poA` a2) && (b1 `poB` b2) )

-- |The dual of a poset
dual :: Poset a -> Poset a
dual (Poset (set, po)) = Poset (set, po')
    where po' x y = po y x


-- ANALYSIS OF POSETS

-- |Given a poset (X,<=), we say that y covers x, written x -< y, if x < y and there is no z in X with x < z < y.
-- The Hasse digraph of a poset is the digraph whose vertices are the elements of the poset,
-- with an edge between every pair (x,y) with x -< y.
-- The Hasse digraph can be represented diagrammatically as a Hasse diagram, by drawing x below y whenever x -< y.
hasseDigraph :: (Eq a) => Poset a -> Digraph a
hasseDigraph (Poset (set,po)) = DG set [(x,y) | x <- set, y <- set, x -< y]
    where x -< y = x /= y && x `po` y && null [z | z <- set, x `po` z, x /= z, z `po` y, z /= y]
-- The partial order can be recovered as the transitive closure of the covers relation
-- !! Can we construct the cover graph without having to compare every pair ??

-- If we know in advance the poset we're interested in,
-- then we're probably better off constructing the Hasse digraph directly

-- (In effect, the transitive closure of the edge relation)
-- |Given a DAG (directed acyclic graph), return the poset consisting of the vertices of the DAG, ordered by reachability.
-- This can be used to recover a poset from its Hasse digraph.
reachabilityPoset :: (Ord a) => Digraph a -> Poset a
reachabilityPoset (DG vs es) = Poset (vs,tc') -- \u v -> tc M.! (u,v)
    where tc = M.fromList [ ((u,v), tc' u v) | u <- vs, v <- vs]
          tc' u v | u == v = True
                  | otherwise = or [tc M.! (w,v) | w <- successors u]
          successors u = [v | (u',v) <- es, u' == u]
-- !! looks like we could memoise more than we are doing


{-
-- For example:
> let poset = posetB 3 in poset == reachabilityPoset (hasseDigraph poset)
True
-}


isOrderPreserving :: (a -> b) -> Poset a -> Poset b -> Bool
isOrderPreserving f (Poset (seta,poa)) (Poset (setb,pob)) =
    and [ x `poa` y == f x `pob` f y | x <- seta, y <- seta ]

-- Find all order isomorphisms between two posets
-- This algorithm is faster to find out whether or not there are any
orderIsos01 (Poset (seta,poa)) (Poset (setb,pob))
    | length seta /= length setb = []
    | otherwise = orderIsos' [] seta setb
    where orderIsos' xys [] [] = [xys]
          orderIsos' xys (x:xs) ys =
              concat [ orderIsos' ((x,y):xys) xs (L.delete y ys)
                     | y <- ys, and [ (x `poa` x', x' `poa` x) == (y `pob` y', y' `pob` y) | (x',y') <- xys ] ]

-- |Are the two posets order-isomorphic?
isOrderIso :: (Eq a, Eq b) => Poset a -> Poset b -> Bool
isOrderIso poseta posetb = (not . null) (orderIsos01 poseta posetb)

-- Find all order isomorphisms between two posets
-- This algorithm is faster to find all isomorphisms, if there are many
-- (It may be that it is faster to find any, for large posets, but the break-even point seems to be quite big)
orderIsos posetA@(Poset (_,poa)) posetB@(Poset (_,pob))
    | map length heightPartA /= map length heightPartB = []
    | otherwise = dfs [] heightPartA heightPartB
    where heightPartA = heightPartitionDAG (hasseDigraph posetA)
          heightPartB = heightPartitionDAG (hasseDigraph posetB)
          dfs xys [] [] = [xys]
          dfs xys ([]:las) ([]:lbs) = dfs xys las lbs
          dfs xys ((x:xs):las) (ys:lbs) =
              concat [ dfs ((x,y):xys) (xs:las) (L.delete y ys : lbs)
                     | y <- ys, and [ (x `poa` x', x' `poa` x) == (y `pob` y', y' `pob` y) | (x',y') <- xys ] ]
-- A variant on this algorithm would use the Hasse digraph rather than the partial order in the test on the last line
-- This might be faster, depending how expensive the partial order comparison function is
-- In effect though, it would then be a DAG isomorphism function

-- The order automorphisms of a poset
orderAuts1 poset = orderIsos poset poset
-- This returns all automorphisms
-- What we really want is to return generators of the permutation group


-- |A linear extension of a poset is a linear ordering of the elements which extends the partial order.
-- Equivalently, it is an ordering [x1..xn] of the underlying set, such that if xi <= xj then i <= j.
isLinext (Poset (set,po)) set' = all (\(x,y) -> not (y `po` x)) (pairs set')


-- |Linear extensions of a poset
linexts (Poset (set,po)) = linexts' [[]] set
    where linexts' lss (r:rs) =
              let lss' = [ lts ++ [r] ++ gts
                         | ls <- lss,
                           let ls' = takeWhile (not . (r `po`)) ls,
                           (lts,gts) <- zip (inits ls') (tails ls),
                           all (not . (`po` r)) gts ]
              in linexts' lss' rs
          linexts' lss [] = lss


