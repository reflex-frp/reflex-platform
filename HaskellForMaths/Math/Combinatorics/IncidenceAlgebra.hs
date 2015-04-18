-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction #-}


module Math.Combinatorics.IncidenceAlgebra where

import Math.Core.Utils

import Math.Combinatorics.Digraph
import Math.Combinatorics.Poset

import Math.Algebra.Field.Base
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures

import Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- INTERVALS IN A POSET

-- |A type to represent an interval in a poset. The (closed) interval [x,y] is the set {z | x <= z <= y} within the poset.
-- Note that the \"empty interval\" is not an interval - that is, the interval [x,y] is only defined for x <= y.
-- The (closed) intervals within a poset form a basis for the incidence algebra as a k-vector space.
data Interval a = Iv (Poset a) (a,a)

instance Eq a => Eq (Interval a) where
    Iv _ (a,b) == Iv _ (a',b') = (a,b) == (a',b')
-- we don't bother to check that they are from the same poset

instance Ord a => Ord (Interval a) where
    compare (Iv _ (a,b)) (Iv _ (a',b')) = compare (a,b) (a',b')

instance Show a => Show (Interval a) where
    show (Iv _ (a,b)) = "Iv (" ++ show a ++ "," ++ show b ++ ")"

{-
-- !! This should probably be called heightPartition not rank
-- rank is only well-defined if we don't have cover edges jumping levels
rankPartition (Iv poset@(Poset (set,po)) (a,b)) = rankPartition' S.empty [a] (L.delete a iv)
    where rankPartition' _ level [] = [level]
          rankPartition' interior boundary exterior =
              let interior' = S.union interior (S.fromList boundary)
                  boundary' = toSet [v | (u,v) <- es, u `elem` boundary, all (`S.member` interior') (predecessors es v)]
                  exterior' = exterior \\ boundary'
              in boundary : rankPartition' interior' boundary' exterior'
          iv = interval poset (a,b)
          (_,es) = coverGraph (Poset (iv,po))
          predecessors es v = [u | (u,v') <- es, v' == v]
-- !! Can be written more efficiently, eg by memoising predecessors and successors, culling covers as we use them, etc.

-- The point of rankPartition function is to enable a slightly faster isomorphism test
-- Could do even better by refining with (indegree, outdegree)
-}

-- The sub-poset defined by an interval
ivPoset (Iv poset@(Poset (_,po)) (x,y)) = Poset (interval poset (x,y), po)

intervalIsos iv1 iv2 = orderIsos (ivPoset iv1) (ivPoset iv2)

isIntervalIso iv1 iv2 = isOrderIso (ivPoset iv1) (ivPoset iv2)
-- we're only really interested in comparing intervals in the same poset

{-
intervalIsoMap1 poset = intervalIsoMap' M.empty [Iv poset xy | xy <- L.sort (intervals poset)]
    where intervalIsoMap' m (iv:ivs) =
              let reps = [iv' | iv' <- M.keys m, m M.! iv' == Nothing, iv `isIntervalIso` iv']
              in if null reps
                 then intervalIsoMap' (M.insert iv Nothing m) ivs
                 else let [iv'] = reps in intervalIsoMap' (M.insert iv (Just iv') m) ivs
          intervalIsoMap' m [] = m
-}

-- A poset on n vertices has at most n(n+1)/2 intervals
-- In the worst case, we might have to compare each interval to all earlier intervals
-- Hence this is O(n^4)
intervalIsoMap poset = isoMap
    where ivs = [Iv poset xy | xy <- intervals poset]
          isoMap = M.fromList [(iv, isoMap' iv) | iv <- ivs]
          isoMap' iv = let reps = [iv' | iv' <- ivs, iv' < iv, isoMap M.! iv' == Nothing, iv `isIntervalIso` iv']
                       in if null reps then Nothing else let [rep] = reps in Just rep
-- Once an interval is identified as a representative, it is likely to take part in many isomorphism tests
-- Whereas most intervals take part in only one
-- So perhaps we could make this more efficient by having an isomorphism test which uses a height partition
-- for the LHS but not for the RHS?

-- |List representatives of the order isomorphism classes of intervals in a poset
intervalIsoClasses :: (Ord a) => Poset a -> [Interval a]
intervalIsoClasses poset = [iv | iv <- M.keys isoMap, isoMap M.! iv == Nothing]
    where isoMap = intervalIsoMap poset 


-- INCIDENCE ALGEBRA

-- |The incidence algebra of a poset is the free k-vector space having as its basis the set of intervals in the poset,
-- with multiplication defined by concatenation of intervals.
-- The incidence algebra can also be thought of as the vector space of functions from intervals to k, with multiplication
-- defined by the convolution (f*g)(x,y) = sum [ f(x,z) g(z,y) | x <= z <= y ].
instance (Eq k, Num k, Ord a) => Algebra k (Interval a) where
    -- |Note that we are not able to give a generic definition of unit for the incidence algebra,
    -- because it depends on which poset we are working in,
    -- and that information is encoded at the value level rather than the type level. See unitIA.
    unit 0 = zero -- so that sum works
    -- unit x = x *> sumv [return (Iv (a,a)) | a <- poset] -- the delta function
    -- but we can't know from the types alone which poset we are working in
    mult = linear mult'
        where mult' (Iv poset (a,b), Iv _ (c,d)) = if b == c then return (Iv poset (a,d)) else zero

-- So multiplication in the incidence algebra is about composition of intervals


-- |The unit of the incidence algebra of a poset
unitIA :: (Eq k, Num k, Ord a) => Poset a -> Vect k (Interval a)
unitIA poset@(Poset (set,_)) = sumv [return (Iv poset (x,x)) | x <- set]

basisIA :: Num k => Poset a -> [Vect k (Interval a)]
basisIA poset = [return (Iv poset xy) | xy <- intervals poset]

-- |The zeta function of a poset
zetaIA :: (Eq k, Num k, Ord a) => Poset a -> Vect k (Interval a)
zetaIA poset = sumv $ basisIA poset

-- Then for example, zeta^2 counts the number of points in each interval
-- See Stanley, Enumerative Combinatorics I, p115ff, for more similar

-- calculate the mobius function of a poset: naive implementation
muIA1 poset@(Poset (set,po)) = sum [mu (x,y) *> return (Iv poset (x,y)) | x <- set, y <- set]
    where mu (x,y) | x == y    = 1
                   | po x y    = negate $ sum [mu (x,z) | z <- set, po x z, po z y, z /= y]
                   | otherwise = 0

-- calculate the mobius function of a poset, with memoization
-- |The Mobius function of a poset
muIA :: (Eq k, Num k, Ord a) => Poset a -> Vect k (Interval a)
muIA poset@(Poset (set,po)) = sumv [mus M.! (x,y) *> return (Iv poset (x,y)) | x <- set, y <- set]
    where mu (x,y) | x == y    = 1
                   | po x y    = negate $ sum [mus M.! (x,z) | z <- set, po x z, po z y, z /= y]
                   | otherwise = 0
          mus = M.fromList [((x,y), mu (x,y)) | x <- set, y <- set] 

-- calculate the inverse of a function in the incidence algebra: naive implementation
invIA1 f | f == zerov = error "invIA 0"
        | any (==0) [f' (x,x) | x <- set] = error "invIA: not invertible"
        | otherwise = g
    where (Iv poset@(Poset (set,po)) _,_) = head $ terms f
          f' (x,y) = coeff (Iv poset (x,y)) f
          g = sumv [g' xy *> return (Iv poset xy) | xy <- intervals poset]
          g' (x,y) | x == y = 1 / f' (x,x)
                   | otherwise = (-1 / f' (x,x)) * sum [f' (x,z) * g' (z,y) | z <- interval poset (x,y), x /= z]

-- Stanley, Enumerative Combinatorics I, p144
-- |The inverse of an element in the incidence algebra of a poset.
-- This is only defined for elements which are non-zero on all intervals (x,x)
invIA :: (Eq k, Fractional k, Ord a) => Vect k (Interval a) -> Maybe (Vect k (Interval a))
invIA f | f == zerov = Nothing -- error "invIA 0"
        | any (==0) [f' (x,x) | x <- set] = Nothing -- error "invIA: not invertible"
        | otherwise = Just g
    where (Iv poset@(Poset (set,po)) _,_) = head $ terms f
          f' (x,y) = coeff (Iv poset (x,y)) f
          g = sumv [g' xy *> return (Iv poset xy) | xy <- intervals poset]
          g' (x,y) | x == y = 1 / f' (x,x)
                   | otherwise = (-1 / f' (x,x)) * sum [f' (x,z) * (g's M.! (z,y)) | z <- interval poset (x,y), x /= z]
          g's = M.fromList [(xy, g' xy) | xy <- intervals poset]

instance (Eq k, Fractional k, Ord a, Show a) => HasInverses (Vect k (Interval a)) where
    inverse f = case invIA f of
                Just g -> g
                Nothing -> error "IncidenceAlgebra.inverse: not invertible"

-- Then for example we can count multichains or chains using the incidence algebra - see Stanley

-- |A function (ie element of the incidence algebra) that counts the total number of chains in each interval
numChainsIA :: (Ord a, Show a) => Poset a -> Vect Q (Interval a)
numChainsIA poset = (2 *> unitIA poset <-> zetaIA poset)^-1

-- The eta function on intervals (x,y) is 1 if x -< y (y covers x), 0 otherwise
etaIA poset = let DG vs es = hasseDigraph poset
              in sumv [return (Iv poset (x,y)) | (x,y) <- es]

-- |A function (ie element of the incidence algebra) that counts the number of maximal chains in each interval
numMaximalChainsIA :: (Ord a, Show a) => Poset a -> Vect Q (Interval a)
numMaximalChainsIA poset = (unitIA poset <-> etaIA poset)^-1


-- In order to quickCheck this, we would need
-- (i) Custom Arbitrary instance - which uses only valid intervals for the poset (ie elts of the basis)
-- (ii) Custom quickCheck property, which uses the correct unit


-- SOME KNOWN MOBIUS FUNCTIONS

muC n = sum [mu' (a,b) *> return (Iv poset (a,b)) | (a,b) <- intervals poset]
    where mu' (a,b) | a == b    =  1
                    | a+1 == b  = -1
                    | otherwise =  0
          poset = chainN n

muB n = sumv [(-1)^(length b - length a) *> return (Iv poset (a,b)) | (a,b) <- intervals poset]
    where poset = posetB n
-- van Lint & Wilson p335

muL n fq = sumv [ ( (-1)^k * q^(k*(k-1) `div` 2) ) *> return (Iv poset (a,b)) |
                  (a,b) <- intervals poset,
                  let k = length b - length a ] -- the difference in dimensions
    where q = length fq
          poset = posetL n fq
-- van Lint & Wilson p335


-- INCIDENCE COALGEBRA
-- Schmitt, Incidence Hopf Algebras

instance (Eq k, Num k, Ord a) => Coalgebra k (Interval a) where
    counit = unwrap . linear counit'
        where counit' (Iv _ (x,y)) = (if x == y then 1 else 0) *> return ()
    comult = linear comult'
        where comult' (Iv poset (x,z)) = sumv [return (Iv poset (x,y), Iv poset (y,z)) | y <- interval poset (x,z)]

-- So comultiplication in the incidence coalgebra is about decomposition of intervals into subintervals


-- But for incidence Hopf algebras, Schmitt wants the basis elts to be isomorphism classes of intervals, not intervals themselves
-- (ie unlabelled intervals)

-- |@toIsoClasses@ is the linear map from the incidence Hopf algebra of a poset to itself,
-- in which each interval is mapped to (the minimal representative of) its isomorphism class.
-- Thus the result can be considered as a linear combination of isomorphism classes of intervals,
-- rather than of intervals themselves.
-- Note that if this operation is to be performed repeatedly for the same poset,
-- then it is more efficient to use @toIsoClasses' poset@, which memoizes the isomorphism class lookup table.
toIsoClasses :: (Eq k, Num k, Ord a) => Vect k (Interval a) -> Vect k (Interval a)
toIsoClasses v
    | v == zerov = zerov
    | otherwise = toIsoClasses' poset v
    where (Iv poset _, _) = head $ terms v

-- |Given a poset, @toIsoClasses' poset@ is the linear map from the incidence Hopf algebra of the poset to itself,
-- in which each interval is mapped to (the minimal representative of) its isomorphism class.
toIsoClasses' :: (Eq k, Num k, Ord a) => Poset a -> Vect k (Interval a) -> Vect k (Interval a)
toIsoClasses' poset = linear isoRep
    where isoRep iv = case isoMap M.! iv of
                      Nothing  -> return iv
                      Just iv' -> return iv'
          isoMap = intervalIsoMap poset


{-
-- for example:

> toIsoClasses $ zetaIA $ posetP 4
15Iv ([[1],[2],[3],[4]],[[1],[2],[3],[4]])+31Iv ([[1],[2],[3],[4]],[[1],[2],[3,4]])+10Iv ([[1],[2],[3],[4]],[[1],[2,3,4]])+3Iv ([[1],[2],[3],[4]],[[1,2],[3,4]])+Iv ([[1],[2],[3],[4]],[[1,2,3,4]])

-- Can we use this to solve "counting squares" problems

> let b3 = comult $ return $ Iv (posetB 3) ([],[1,2,3])
> let isoB3 = toIsoClasses' $ posetB 3
> (isoB3 `tf` isoB3) b3
(Iv ([],[]),Iv ([],[1,2,3]))+3(Iv ([],[1]),Iv ([],[1,2]))+3(Iv ([],[1,2]),Iv ([],[1]))+(Iv ([],[1,2,3]),Iv ([],[]))

-- The incidence coalgebra of the binomial poset is isomorphic to the binomial coalgebra

-- if we just want to get the coefficients, we don't need to use comult:

> let poset@(Poset (set,po)) = posetB 3 in toIsoClasses $ sumv [return (Iv poset ([],x)) | x <- set]
Iv ([],[])+3Iv ([],[1])+3Iv ([],[1,2])+Iv ([],[1,2,3])

> let n = 4; p  = comult $ return $ Iv (posetP n) ([[i] | i<- [1..n]],[[1..n]]); iso = toIsoClasses' (posetP n) in (iso `tf` iso) p
(Iv ([[1],[2],[3],[4]],[[1],[2],[3],[4]]),Iv ([[1],[2],[3],[4]],[[1,2,3,4]]))+
6(Iv ([[1],[2],[3],[4]],[[1],[2],[3,4]]),Iv ([[1],[2],[3],[4]],[[1],[2,3,4]]))+
4(Iv ([[1],[2],[3],[4]],[[1],[2,3,4]]),Iv ([[1],[2],[3],[4]],[[1],[2],[3,4]]))+
3(Iv ([[1],[2],[3],[4]],[[1,2],[3,4]]),Iv ([[1],[2],[3],[4]],[[1],[2],[3,4]]))+
(Iv ([[1],[2],[3],[4]],[[1,2,3,4]]),Iv ([[1],[2],[3],[4]],[[1],[2],[3],[4]]))

-- These are multinomial coefficients, OEIS A178867: 1; 1,1; 1,3,1; 1,6,4,3,1; 1,10,10,15,5,10,1; ...
-- Although A036040, which is the same up to ordering, seems a better match. (Our order is fairly arbitrary)

> let n = 4; p  = comult $ return $ Iv (posetL n f2) ([],[[1 :: F2,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]); iso = toIsoClasses' (posetL n f2) in (iso `tf` iso) p
(Iv ([],[]),Iv ([],[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]))+
15(Iv ([],[[0,0,0,1]]),Iv ([],[[0,1,0,0],[0,0,1,0],[0,0,0,1]]))+
35(Iv ([],[[0,0,1,0],[0,0,0,1]]),Iv ([],[[0,0,1,0],[0,0,0,1]]))+
15(Iv ([],[[0,1,0,0],[0,0,1,0],[0,0,0,1]]),Iv ([],[[0,0,0,1]]))+
(Iv ([],[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]),Iv ([],[]))

-- With L n fq, we get the q-binomial coefficients, eg OEIS A022166:
1; 1, 1; 1, 3, 1; 1, 7, 7, 1; 1, 15, 35, 15, 1
-}


-- This still isn't quite what Schmitt wants
-- Schmitt, IHA, p6
-- The incidence Hopf algebra should have as its basis isomorphism classes of intervals, not intervals
-- The mult is defined as direct product of posets
