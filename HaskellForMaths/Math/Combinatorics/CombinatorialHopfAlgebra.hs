-- Copyright (c) 2012, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction, ScopedTypeVariables, DeriveFunctor #-}

-- |A module defining the following Combinatorial Hopf Algebras, together with coalgebra or Hopf algebra morphisms between them:
--
-- * Sh, the Shuffle Hopf algebra
--
-- * SSym, the Malvenuto-Reutnenauer Hopf algebra of permutations
--
-- * YSym, the (dual of the) Loday-Ronco Hopf algebra of binary trees
--
-- * QSym, the Hopf algebra of quasi-symmetric functions (having a basis indexed by compositions)
--
-- * Sym, the Hopf algebra of symmetric functions (having a basis indexed by integer partitions)
--
-- * NSym, the Hopf algebra of non-commutative symmetric functions
module Math.Combinatorics.CombinatorialHopfAlgebra where

-- Sources:

-- Structure of the Malvenuto-Reutenauer Hopf algebra of permutations
-- Marcelo Aguiar and Frank Sottile
-- http://www.math.tamu.edu/~sottile/research/pdf/SSym.pdf

-- Structure of the Loday-Ronco Hopf algebra of trees
-- Marcelo Aguiar and Frank Sottile
-- http://www.math.tamu.edu/~sottile/research/pdf/Loday.pdf

-- Hopf Structures on the Multiplihedra
-- Stefan Forcey, Aaron Lauve and Frank Sottile
-- http://www.math.tamu.edu/~sottile/research/pdf/MSym.pdf

-- Lie Algebras and Hopf Algebras
-- Michiel Hazewinkel, Nadiya Gubareni, V.V.Kirichenko

import Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Set as S

import Math.Core.Field
import Math.Core.Utils

import Math.Algebras.VectorSpace hiding (E)
import Math.Algebras.TensorProduct
import Math.Algebras.Structures

import Math.Combinatorics.Poset

-- import Math.Algebra.Group.PermutationGroup
import Math.CommutativeAlgebra.Polynomial


-- SHUFFLE ALGEBRA
-- This is just the tensor algebra, but with shuffle product (and deconcatenation coproduct)

-- |A basis for the shuffle algebra. As a vector space, the shuffle algebra is identical to the tensor algebra.
-- However, we consider a different algebra structure, based on the shuffle product. Together with the
-- deconcatenation coproduct, this leads to a Hopf algebra structure.
newtype Shuffle a = Sh [a] deriving (Eq,Ord,Show)

-- |Construct a basis element of the shuffle algebra
sh :: [a] -> Vect Q (Shuffle a)
sh = return . Sh

shuffles (x:xs) (y:ys) = map (x:) (shuffles xs (y:ys)) ++ map (y:) (shuffles (x:xs) ys)
shuffles xs [] = [xs]
shuffles [] ys = [ys]

instance (Eq k, Num k, Ord a) => Algebra k (Shuffle a) where
    unit x = x *> return (Sh [])
    mult = linear mult' where
        mult' (Sh xs, Sh ys) = sumv [return (Sh zs) | zs <- shuffles xs ys]

deconcatenations xs = zip (inits xs) (tails xs)

instance (Eq k, Num k, Ord a) => Coalgebra k (Shuffle a) where
    counit = unwrap . linear counit' where counit' (Sh xs) = if null xs then 1 else 0
    comult = linear comult' where
        comult' (Sh xs) = sumv [return (Sh us, Sh vs) | (us, vs) <- deconcatenations xs]

instance (Eq k, Num k, Ord a) => Bialgebra k (Shuffle a) where {}

instance (Eq k, Num k, Ord a) => HopfAlgebra k (Shuffle a) where
    antipode = linear (\(Sh xs) -> (-1)^length xs *> return (Sh (reverse xs)))


-- SSYM: PERMUTATIONS
-- (This is permutations considered as combinatorial objects rather than as algebraic objects)

-- Permutations with shifted shuffle product and flattened deconcatenation coproduct
-- This is the Malvenuto-Reutenauer Hopf algebra of permutations, SSym.
-- It is neither commutative nor co-commutative

-- ssymF xs is the fundamental basis F_xs (Aguiar and Sottile)

-- |The fundamental basis for the Malvenuto-Reutenauer Hopf algebra of permutations, SSym.
newtype SSymF = SSymF [Int] deriving (Eq)

instance Ord SSymF where
    compare (SSymF xs) (SSymF ys) = compare (length xs, xs) (length ys, ys)

instance Show SSymF where
    show (SSymF xs) = "F " ++ show xs

-- |Construct a fundamental basis element in SSym.
-- The list of ints must be a permutation of [1..n], eg [1,2], [3,4,2,1].
ssymF :: [Int] -> Vect Q SSymF
ssymF xs | L.sort xs == [1..n] = return (SSymF xs)
         | otherwise = error "Not a permutation of [1..n]"
         where n = length xs

-- so this is a candidate mult. It is associative and SSymF [] is obviously a left and right identity
-- (need quickcheck properties to prove that)
shiftedConcat (SSymF xs) (SSymF ys) = let k = length xs in SSymF (xs ++ map (+k) ys)

prop_Associative f (x,y,z) = f x (f y z) == f (f x y) z

-- > quickCheck (prop_Associative shiftedConcat)
-- +++ OK, passed 100 tests.


instance (Eq k, Num k) => Algebra k SSymF where
    unit x = x *> return (SSymF [])
    mult = linear mult' where
        mult' (SSymF xs, SSymF ys) =
            let k = length xs
            in sumv [return (SSymF zs) | zs <- shuffles xs (map (+k) ys)]


-- standard permutation, also called flattening, eg [6,2,5] -> [3,1,2]
flatten xs = let mapping = zip (L.sort xs) [1..]
        in [y | x <- xs, let Just y = lookup x mapping] 

instance (Eq k, Num k) => Coalgebra k SSymF where
    counit = unwrap . linear counit' where counit' (SSymF xs) = if null xs then 1 else 0
    comult = linear comult'
        where comult' (SSymF xs) = sumv [return (SSymF (st us), SSymF (st vs)) | (us, vs) <- deconcatenations xs]
              st = flatten

instance (Eq k, Num k) => Bialgebra k SSymF where {}

instance (Eq k, Num k) => HopfAlgebra k SSymF where
    antipode = linear antipode' where
        antipode' (SSymF []) = return (SSymF [])
        antipode' x@(SSymF xs) = (negatev . mult . (id `tf` antipode) . removeTerm (SSymF [],x) . comult . return) x
        -- This expression for antipode is derived from mult . (id `tf` antipode) . comult == unit . counit
        -- It's possible because this is a graded, connected Hopf algebra. (connected means the counit is projection onto the grade 0 part)
-- It would be nicer to have an explicit expression for antipode.
{-
instance (Eq k, Num k) => HopfAlgebra k SSymF where
    antipode = linear antipode'
        where antipode' (SSymF v) = sumv [lambda v w *> return (SSymF w) | w <- L.permutations v]
              lambda v w = length [s | s <- powerset [1..n-1],  odd (length s), descentSet (w^-1 * v_s) `isSubset` s]
                         - length [s | s <- powerset [1..n-1],  even (length s), descentSet (w^-1 * v_s) `isSubset` s]
-}

instance HasInverses SSymF where
    inverse (SSymF xs) = SSymF $ map snd $ L.sort $ map (\(s,t)->(t,s)) $ zip [1..] xs

-- Hazewinkel p267
-- |A pairing showing that SSym is self-adjoint
instance (Eq k, Num k) => HasPairing k SSymF SSymF where
    pairing = linear pairing' where
        pairing' (x,y) = delta x (inverse y)
-- Not entirely clear to me why this works
-- The pairing is *not* positive definite (Hazewinkel p267)
-- eg (\x -> pairing' x x >= 0) (ssymF [1,3,2] + ssymF [2,3,1] - ssymF [3,1,2]) == False


-- |An alternative \"monomial\" basis for the Malvenuto-Reutenauer Hopf algebra of permutations, SSym.
-- This basis is related to the fundamental basis by Mobius inversion in the poset of permutations with the weak order.
newtype SSymM = SSymM [Int] deriving (Eq)

instance Ord SSymM where
    compare (SSymM xs) (SSymM ys) = compare (length xs, xs) (length ys, ys)

instance Show SSymM where
    show (SSymM xs) = "M " ++ show xs

-- |Construct a monomial basis element in SSym.
-- The list of ints must be a permutation of [1..n], eg [1,2], [3,4,2,1].
ssymM :: [Int] -> Vect Q SSymM
ssymM xs | L.sort xs == [1..n] = return (SSymM xs)
         | otherwise = error "Not a permutation of [1..n]"
         where n = length xs

inversions xs = let ixs = zip [1..] xs
                in [(i,j) | ((i,xi),(j,xj)) <- pairs ixs, xi > xj]

-- should really check that xs and ys have the same length, and perhaps insist also on same type
weakOrder xs ys = inversions xs `isSubsetAsc` inversions ys

mu (set,po) x y = mu' x y where
    mu' x y | x == y    = 1
            | po x y    = negate $ sum [mu' x z | z <- set, po x z, po z y, z /= y]
            | otherwise = 0

-- |Convert an element of SSym represented in the monomial basis to the fundamental basis
ssymMtoF :: (Eq k, Num k) => Vect k SSymM -> Vect k SSymF
ssymMtoF = linear ssymMtoF'
    where ssymMtoF' (SSymM u) = sumv [mu (set,po) u v *> return (SSymF v) | v <- set, po u v]
              where set = L.permutations u
                    po = weakOrder

-- |Convert an element of SSym represented in the fundamental basis to the monomial basis
ssymFtoM :: (Eq k, Num k) => Vect k SSymF -> Vect k SSymM
ssymFtoM = linear ssymFtoM'
    where ssymFtoM' (SSymF u) = sumv [return (SSymM v) | v <- set, po u v]
              where set = L.permutations u
                    po = weakOrder

-- (p,q)-shuffles: permutations of [1..p+q] having at most one descent, at position p
-- denoted S^{(p,q)} in Aguiar&Sottile
-- (Grassmannian permutations?)
-- pqShuffles p q = [u++v | u <- combinationsOf p [1..n], let v = [1..n] `diffAsc` u] where n = p+q

-- The inverse of a (p,q)-shuffle.
-- The special form of (p,q)-shuffles makes an O(n) algorithm possible
-- pqInverse :: Int -> Int -> [Int] -> [Int]
{-
-- incorrect
pqInverse p q xs = pqInverse' [1..p] [p+1..p+q] xs
    where pqInverse' (l:ls) (r:rs) (x:xs) =
              if x <= p then l : pqInverse' ls (r:rs) xs else r : pqInverse' (l:ls) rs xs
          pqInverse' ls rs _ = ls ++ rs -- one of them is null
-}
-- pqInverseShuffles p q = shuffles [1..p] [p+1..p+q]


instance (Eq k, Num k) => Algebra k SSymM where
    unit x = x *> return (SSymM [])
    mult = ssymFtoM . mult . (ssymMtoF `tf` ssymMtoF)

{-
mult2 = linear mult'
    where mult' (SSymM u, SSymM v) = sumv [alpha u v w *> return (SSymM w) | w <- L.permutations [1..p+q] ]
                                     where p = length u; q = length v

alpha u v w = length [z | z <- pqInverseShuffles p q, let uv = shiftedConcat u v,
                          uv * z `weakOrder` w, u and v are maximal, ie no transposition of adjacents in either also works]
    where p = length u
          q = length v
-- so we need to define (*) for permutations in row form
-}

instance (Eq k, Num k) => Coalgebra k SSymM where
    counit = unwrap . linear counit' where counit' (SSymM xs) = if null xs then 1 else 0
    -- comult = (ssymFtoM `tf` ssymFtoM) . comult . ssymMtoF
    comult = linear comult'
        where comult' (SSymM xs) = sumv [return (SSymM (flatten ys), SSymM (flatten zs))
                                        | (ys,zs) <- deconcatenations xs,
                                          minimum (infinity:ys) > maximum (0:zs)] -- ie deconcatenations at a global descent
              infinity = maxBound :: Int

instance (Eq k, Num k) => Bialgebra k SSymM where {}

instance (Eq k, Num k) => HopfAlgebra k SSymM where
    antipode = ssymFtoM . antipode . ssymMtoF


-- Hazewinkel p265
instance (Eq k, Num k) => Algebra k (Dual SSymF) where
    unit x = x *> return (Dual (SSymF []))
    mult = linear mult' where
        mult' (Dual (SSymF xs), Dual (SSymF ys)) =
            sumv [(return . Dual . SSymF) (xs'' ++ ys'')
                 | xs' <- combinationsOf r [1..r+s], let ys' = diffAsc [1..r+s] xs',
                   xs'' <- L.permutations xs', flatten xs'' == xs,
                   ys'' <- L.permutations ys', flatten ys'' == ys ]
            where r = length xs; s = length ys
-- In other words, mult x y is the sum of those z whose comult (in SSymF) has an (x,y) term
-- So the matrix for mult is the transpose of the matrix for comult in SSymF

instance (Eq k, Num k) => Coalgebra k (Dual SSymF) where
    counit = unwrap . linear counit' where counit' (Dual (SSymF xs)) = if null xs then 1 else 0
    comult = linear comult' where
        comult' (Dual (SSymF xs)) =
            sumv [return (Dual (SSymF ys), Dual (SSymF (flatten zs))) | i <- [0..n], let (ys,zs) = L.partition (<=i) xs ]
            where n = length xs
-- In other words, comult x is the sum of those (y,z) whose mult (in SSymF) has a z term
-- So the matrix for comult is the transpose of the matrix for mult in SSymF

instance (Eq k, Num k) => Bialgebra k (Dual SSymF) where {}

instance (Eq k, Num k) => HopfAlgebra k (Dual SSymF) where
    antipode = linear antipode' where
        antipode' (Dual (SSymF [])) = return (Dual (SSymF []))
        antipode' x@(Dual (SSymF xs)) =
            (negatev . mult . (id `tf` antipode) . removeTerm (Dual (SSymF []),x) . comult . return) x

-- This pairing is positive definite (Hazewinkel p267)
instance (Eq k, Num k) => HasPairing k SSymF (Dual SSymF) where
    pairing = linear pairing' where
        pairing' (x, Dual y) = delta x y

-- |The isomorphism from SSym to its dual that takes a permutation in the fundamental basis to its inverse in the dual basis
ssymFtoDual :: (Eq k, Num k) => Vect k SSymF -> Vect k (Dual SSymF)
ssymFtoDual = nf . fmap (Dual . inverse)
-- This is theta on Hazewinkel p266 (though later he also uses theta for the inverse of this map)


-- YSYM: PLANAR BINARY TREES
-- These are really rooted planar binary trees.
-- It's because they're planar that we can distinguish left and right child branches.
-- (Non-planar would be if we considered trees where left and right children are swapped relative to one another as the same tree)
-- It is neither commutative nor co-commutative

-- |A type for (rooted) planar binary trees. The basis elements of the Loday-Ronco Hopf algebra are indexed by these.
--
-- Although the trees are labelled, we're really only interested in the shapes of the trees, and hence in the type PBT ().
-- The Algebra, Coalgebra and HopfAlgebra instances all ignore the labels.
-- However, it is convenient to allow labels, as they can be useful for seeing what is going on, and they also make it possible
-- to define various ways to create trees from lists of labels.
data PBT a = T (PBT a) a (PBT a) | E deriving (Eq, Show, Functor)

instance Ord a => Ord (PBT a) where
    compare u v = compare (shapeSignature u, prefix u) (shapeSignature v, prefix v)

-- |The fundamental basis for (the dual of) the Loday-Ronco Hopf algebra of binary trees, YSym.
newtype YSymF a = YSymF (PBT a) deriving (Eq, Ord, Functor)

instance Show a => Show (YSymF a) where
    show (YSymF t) = "F(" ++ show t ++ ")"

-- |Construct the element of YSym in the fundamental basis indexed by the given tree
ysymF :: PBT a -> Vect Q (YSymF a)
ysymF t = return (YSymF t)

{-
depth (T l x r) = 1 + max (depth l) (depth r)
depth E = 0
-}
nodecount (T l x r) = 1 + nodecount l + nodecount r
nodecount E = 0

-- in fact leafcount t = 1 + nodecount t (easiest to see with a picture)
leafcount (T l x r) = leafcount l + leafcount r
leafcount E = 1

prefix E = []
prefix (T l x r) = x : prefix l ++ prefix r

-- The shape signature uniquely identifies the shape of a tree.
-- Trees with distinct shapes have distinct signatures.
-- In addition, if sorting on shapeSignature, smaller trees sort before larger trees,
-- and leftward leaning trees sort before rightward leaning trees
shapeSignature t = shapeSignature' (nodeCountTree t)
    where shapeSignature' E = [0] -- not [], otherwise we can't distinguish T (T E () E) () E from T E () (T E () E)
          shapeSignature' (T l x r) = x : shapeSignature' r ++ shapeSignature' l

nodeCountTree E = E
nodeCountTree (T l _ r) = T l' n r'
    where l' = nodeCountTree l
          r' = nodeCountTree r
          n = 1 + (case l' of E -> 0; T _ lc _ -> lc) + (case r' of E -> 0; T _ rc _ -> rc)

leafCountTree E = E
leafCountTree (T l _ r) = T l' n r'
    where l' = leafCountTree l
          r' = leafCountTree r
          n = (case l' of E -> 1; T _ lc _ -> lc) + (case r' of E -> 1; T _ rc _ -> rc)

-- A tree that counts nodes in left and right subtrees
lrCountTree E = E
lrCountTree (T l _ r) = T l' (lc,rc) r'
    where l' = lrCountTree l
          r' = lrCountTree r
          lc = case l' of E -> 0; T _ (llc,lrc) _ -> 1 + llc + lrc
          rc = case r' of E -> 0; T _ (rlc,rrc) _ -> 1 + rlc + rrc

shape :: PBT a -> PBT ()
shape t = fmap (\_ -> ()) t

-- label the nodes of a tree in infix order while preserving its shape
numbered t = numbered' 1 t
    where numbered' _ E = E
          numbered' i (T l x r) = let k = nodecount l in T (numbered' i l) (i+k) (numbered' (i+k+1) r)
-- could also pair the numbers with the input labels


splits E = [(E,E)]
splits (T l x r) = [(u, T v x r) | (u,v) <- splits l] ++ [(T l x u, v) | (u,v) <- splits r]

instance (Eq k, Num k, Ord a) => Coalgebra k (YSymF a) where
    counit = unwrap . linear counit' where counit' (YSymF E) = 1; counit' (YSymF (T _ _ _)) = 0
    comult = linear comult'
        where comult' (YSymF t) = sumv [return (YSymF u, YSymF v) | (u,v) <- splits t]
              -- using sumv rather than sum to avoid requiring Show a
    -- so again this is a kind of deconcatenation coproduct

multisplits 1 t = [ [t] ]
multisplits 2 t = [ [u,v] | (u,v) <- splits t ]
multisplits n t = [ u:ws | (u,v) <- splits t, ws <- multisplits (n-1) v ]

graft [t] E = t
graft ts (T l x r) = let (ls,rs) = splitAt (leafcount l) ts
                     in T (graft ls l) x (graft rs r)

instance (Eq k, Num k, Ord a) => Algebra k (YSymF a) where
    unit x = x *> return (YSymF E)
    mult = linear mult' where
        mult' (YSymF t, YSymF u) = sumv [return (YSymF (graft ts u)) | ts <- multisplits (leafcount u) t]
        -- using sumv rather than sum to avoid requiring Show a

instance (Eq k, Num k, Ord a) => Bialgebra k (YSymF a) where {}

instance (Eq k, Num k, Ord a) => HopfAlgebra k (YSymF a) where
    antipode = linear antipode' where
        antipode' (YSymF E) = return (YSymF E)
        antipode' x = (negatev . mult . (id `tf` antipode) . removeTerm (YSymF E,x) . comult . return) x


-- |An alternative \"monomial\" basis for (the dual of) the Loday-Ronco Hopf algebra of binary trees, YSym.
newtype YSymM = YSymM (PBT ()) deriving (Eq, Ord)

instance Show YSymM where
    show (YSymM t) = "M(" ++ show t ++ ")"

-- |Construct the element of YSym in the monomial basis indexed by the given tree
ysymM :: PBT () -> Vect Q YSymM
ysymM t = return (YSymM t)

-- |List all trees with the given number of nodes
trees :: Int -> [PBT ()]
trees 0 = [E]
trees n = [T l () r | i <- [0..n-1], l <- trees (n-1-i), r <- trees i]

-- |The covering relation for the Tamari partial order on binary trees
tamariCovers :: PBT a -> [PBT a]
tamariCovers E = []
tamariCovers (T t@(T u x v) y w) = [T t' y w | t' <- tamariCovers t]
                                ++ [T t y w' | w' <- tamariCovers w]
                                ++ [T u y (T v x w)]
                                -- Note that this preserves the descending property, and hence the bijection with permutations
                                -- If we were to swap x and y, we would preserve the binary search tree property instead (if our trees had it)
tamariCovers (T E x u) = [T E x u' | u' <- tamariCovers u]  

-- |The up-set of a binary tree in the Tamari partial order
tamariUpSet :: Ord a => PBT a -> [PBT a]
tamariUpSet t = upSet' [] [t]
    where upSet' interior boundary =
              if null boundary
              then interior
              else let interior' = setUnionAsc interior boundary
                       boundary' = toSet $ concatMap tamariCovers boundary
                   in upSet' interior' boundary'

-- tamariOrder1 u v = v `elem` upSet u

-- |The Tamari partial order on binary trees.
-- This is only defined between trees of the same size (number of nodes).
-- The result between trees of different sizes is undefined (we don't check).
tamariOrder :: PBT a -> PBT a -> Bool
tamariOrder u v = weakOrder (minPerm u) (minPerm v)
-- It should be possible to unpack this to be a statement purely about trees, but probably not worth it

-- |Convert an element of YSym represented in the monomial basis to the fundamental basis
ysymMtoF :: (Eq k, Num k) => Vect k YSymM -> Vect k (YSymF ())
ysymMtoF = linear ysymMtoF'
    where ysymMtoF' (YSymM t) = sumv [mu (set,po) t s *> return (YSymF s) | s <- set]
              where po = tamariOrder
                    set = tamariUpSet t -- [s | s <- trees (nodecount t), t `tamariOrder` s]

-- |Convert an element of YSym represented in the fundamental basis to the monomial basis
ysymFtoM :: (Eq k, Num k) => Vect k (YSymF ()) -> Vect k YSymM
ysymFtoM = linear ysymFtoM'
    where ysymFtoM' (YSymF t) = sumv [return (YSymM s) | s <- tamariUpSet t]
                            -- sumv [return (YSymM s) | s <- trees (nodecount t), t `tamariOrder` s]


instance (Eq k, Num k) => Algebra k YSymM where
    unit x = x *> return (YSymM E)
    mult = ysymFtoM . mult . (ysymMtoF `tf` ysymMtoF)

instance (Eq k, Num k) => Coalgebra k YSymM where
    counit = unwrap . linear counit' where counit' (YSymM E) = 1; counit' (YSymM (T _ _ _)) = 0
    -- comult = (ysymFtoM `tf` ysymFtoM) . comult . ysymMtoF
    comult = linear comult' where
        comult' (YSymM t) = sumv [return (YSymM r, YSymM s) | (rs,ss) <- deconcatenations (underDecomposition t),
                                                              let r = foldl under E rs, let s = foldl under E ss]


instance (Eq k, Num k) => Bialgebra k YSymM where {}

instance (Eq k, Num k) => HopfAlgebra k YSymM where
    antipode = ysymFtoM . antipode . ysymMtoF 


-- QSYM: QUASI-SYMMETRIC FUNCTIONS
-- The following is the Hopf algebra QSym of quasi-symmetric functions
-- using the monomial and fundamental bases (indexed by compositions)

-- compositions in ascending order
-- might be better to use bfs to get length order
-- |List the compositions of an integer n. For example, the compositions of 4 are [[1,1,1,1],[1,1,2],[1,2,1],[1,3],[2,1,1],[2,2],[3,1],[4]]
compositions :: Int -> [[Int]]
compositions 0 = [[]]
compositions n = [i:is | i <- [1..n], is <- compositions (n-i)]

-- can retrieve subsets of [1..n-1] from compositions n as follows
-- > map (tail . scanl (+) 0) (map init $ compositions 4)
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- quasi shuffles of two compositions
quasiShuffles :: [Int] -> [Int] -> [[Int]]
quasiShuffles (x:xs) (y:ys) = map (x:) (quasiShuffles xs (y:ys)) ++
                              map ((x+y):) (quasiShuffles xs ys) ++
                              map (y:) (quasiShuffles (x:xs) ys)
quasiShuffles xs [] = [xs]
quasiShuffles [] ys = [ys]


-- |A type for the monomial basis for the quasi-symmetric functions, indexed by compositions.
newtype QSymM = QSymM [Int] deriving (Eq)

instance Ord QSymM where
    compare (QSymM xs) (QSymM ys) = compare (sum xs, xs) (sum ys, ys)

instance Show QSymM where
    show (QSymM xs) = "M " ++ show xs

-- |Construct the element of QSym in the monomial basis indexed by the given composition
qsymM :: [Int] -> Vect Q QSymM
qsymM xs | all (>0) xs = return (QSymM xs)
         | otherwise = error "qsymM: not a composition"

instance (Eq k, Num k) => Algebra k QSymM where
    unit x = x *> return (QSymM [])
    mult = linear mult' where
        mult' (QSymM alpha, QSymM beta) = sumv [return (QSymM gamma) | gamma <- quasiShuffles alpha beta]

instance (Eq k, Num k) => Coalgebra k QSymM where
    counit = unwrap . linear counit' where counit' (QSymM alpha) = if null alpha then 1 else 0
    comult = linear comult' where
        comult' (QSymM gamma) = sumv [return (QSymM alpha, QSymM beta) | (alpha,beta) <- deconcatenations gamma]

instance (Eq k, Num k) => Bialgebra k QSymM where {}

instance (Eq k, Num k) => HopfAlgebra k QSymM where
    antipode = linear antipode' where
        antipode' (QSymM alpha) = (-1)^length alpha * sumv [return (QSymM beta) | beta <- coarsenings (reverse alpha)]
        -- antipode' (QSymM alpha) = (-1)^length alpha * sumv [return (QSymM (reverse beta)) | beta <- coarsenings alpha]

coarsenings (x1:x2:xs) = map (x1:) (coarsenings (x2:xs)) ++ coarsenings ((x1+x2):xs)
coarsenings xs = [xs] -- for xs a singleton or null

refinements (x:xs) = [y++ys | y <- compositions x, ys <- refinements xs]
refinements [] = [[]]


-- |A type for the fundamental basis for the quasi-symmetric functions, indexed by compositions.
newtype QSymF = QSymF [Int] deriving (Eq)

instance Ord QSymF where
    compare (QSymF xs) (QSymF ys) = compare (sum xs, xs) (sum ys, ys)

instance Show QSymF where
    show (QSymF xs) = "F " ++ show xs

-- |Construct the element of QSym in the fundamental basis indexed by the given composition
qsymF :: [Int] -> Vect Q QSymF
qsymF xs | all (>0) xs = return (QSymF xs)
         | otherwise = error "qsymF: not a composition"

-- |Convert an element of QSym represented in the monomial basis to the fundamental basis
qsymMtoF :: (Eq k, Num k) => Vect k QSymM -> Vect k QSymF
qsymMtoF = linear qsymMtoF'
    where qsymMtoF' (QSymM alpha) = sumv [(-1) ^ (length beta - length alpha) *> return (QSymF beta) | beta <- refinements alpha]

-- |Convert an element of QSym represented in the fundamental basis to the monomial basis
qsymFtoM :: (Eq k, Num k) => Vect k QSymF -> Vect k QSymM
qsymFtoM = linear qsymFtoM'
    where qsymFtoM' (QSymF alpha) = sumv [return (QSymM beta) | beta <- refinements alpha] -- ie beta <- up-set of alpha

instance (Eq k, Num k) => Algebra k QSymF where
    unit x = x *> return (QSymF [])
    mult = qsymMtoF . mult . (qsymFtoM `tf` qsymFtoM)

instance (Eq k, Num k) => Coalgebra k QSymF where
    counit = unwrap . linear counit' where counit' (QSymF xs) = if null xs then 1 else 0
    comult = (qsymMtoF `tf` qsymMtoF) . comult . qsymFtoM

instance (Eq k, Num k) => Bialgebra k QSymF where {}

instance (Eq k, Num k) => HopfAlgebra k QSymF where
    antipode = qsymMtoF . antipode . qsymFtoM


-- QUASI-SYMMETRIC POLYNOMIALS

-- the above induces Hopf algebra structure on quasi-symmetric functions via
-- m_alpha -> sum [product (zipWith (^) (map x_ is) alpha | is <- combinationsOf k [] ] where k = length alpha

xvars n = [glexvar ("x" ++ show i) | i <- [1..n] ]

-- |@qsymPoly n is@ is the quasi-symmetric polynomial in n variables for the indices is. (This corresponds to the
-- monomial basis for QSym.) For example, qsymPoly 3 [2,1] == x1^2*x2+x1^2*x3+x2^2*x3.
qsymPoly :: Int -> [Int] -> GlexPoly Q String
qsymPoly n is = sum [product (zipWith (^) xs' is) | xs' <- combinationsOf r xs]
    where xs = xvars n
          r = length is


-- SYM, THE HOPF ALGEBRA OF SYMMETRIC FUNCTIONS

-- |A type for the monomial basis for Sym, the Hopf algebra of symmetric functions, indexed by integer partitions
newtype SymM = SymM [Int] deriving (Eq,Show)

instance Ord SymM where
    compare (SymM xs) (SymM ys) = compare (sum xs, ys) (sum ys, xs) -- note the order reversal in snd

-- |Construct the element of Sym in the monomial basis indexed by the given integer partition
symM :: [Int] -> Vect Q SymM
symM xs | all (>0) xs = return (SymM $ sortDesc xs)
        | otherwise = error "symM: not a partition"

instance (Eq k, Num k) => Algebra k SymM where
    unit x = x *> return (SymM [])
    mult = linear mult' where
        mult' (SymM lambda, SymM mu) = sumv [return (SymM nu) | nu <- symMult lambda mu]

-- multisetPermutations = toSet . L.permutations

-- compositionsFromPartition2 = foldl (\xss ys -> concatMap (shuffles ys) xss) [[]] . L.group
-- compositionsFromPartition2 = foldl (\ls r -> concat [shuffles l r | l <- ls]) [[]] . L.group

-- The partition must be in either ascending or descending order (so that L.group does as expected)
compositionsFromPartition = foldr (\l rs -> concatMap (shuffles l) rs) [[]] . L.group

-- In effect, we multiply in Sym by converting to QSym, multiplying there, and converting back.
-- It would be nice to find a more direct method.
symMult xs ys = filter isWeaklyDecreasing $ concat
    [quasiShuffles xs' ys' | xs' <- compositionsFromPartition xs, ys' <- compositionsFromPartition ys]

instance (Eq k, Num k) => Coalgebra k SymM where
    counit = unwrap . linear counit' where counit' (SymM lambda) = if null lambda then 1 else 0
    comult = linear comult' where
        comult' (SymM lambda) = sumv [return (SymM mu, SymM nu) | mu <- toSet (powersetdfs lambda), let nu = diffDesc lambda mu]

instance (Eq k, Num k) => Bialgebra k SymM where {}

instance (Eq k, Num k) => HopfAlgebra k SymM where
    antipode = linear antipode' where
        antipode' (SymM []) = return (SymM [])
        antipode' x = (negatev . mult . (id `tf` antipode) . removeTerm (SymM [],x) . comult . return) x


-- |The elementary basis for Sym, the Hopf algebra of symmetric functions. Defined informally as
-- > symE [n] = symM (replicate n 1)
-- > symE lambda = product [symE [p] | p <- lambda]
newtype SymE = SymE [Int] deriving (Eq,Ord,Show)

symE :: [Int] -> Vect Q SymE
symE xs | all (>0) xs = return (SymE $ sortDesc xs)
        | otherwise = error "symE: not a partition"

instance (Eq k, Num k) => Algebra k SymE where
    unit x = x *> return (SymE [])
    mult = linear (\(SymE lambda, SymE mu) -> return $ SymE $ multisetSumDesc lambda mu)

instance (Eq k, Num k) => Coalgebra k SymE where
    counit = unwrap . linear counit' where counit' (SymE lambda) = if null lambda then 1 else 0
    comult = linear comult' where
        comult' (SymE [n]) = sumv [return (e i, e (n-i)) | i <- [0..n] ]
        comult' (SymE lambda) = product [comult' (SymE [n]) | n <- lambda]
        e 0 = SymE []
        e i = SymE [i]

instance (Eq k, Num k) => Bialgebra k SymE where {}

-- |Convert from the elementary to the monomial basis of Sym
symEtoM :: (Eq k, Num k) => Vect k SymE -> Vect k SymM
symEtoM = linear symEtoM' where
    symEtoM' (SymE [n]) = return (SymM (replicate n 1))
    symEtoM' (SymE lambda) = product [symEtoM' (SymE [p]) | p <- lambda]


-- |The complete basis for Sym, the Hopf algebra of symmetric functions. Defined informally as
-- > symH [n] = sum [symM lambda | lambda <- integerPartitions n] -- == all monomials of weight n
-- > symH lambda = product [symH [p] | p <- lambda]
newtype SymH = SymH [Int] deriving (Eq,Ord,Show)

symH :: [Int] -> Vect Q SymH
symH xs | all (>0) xs = return (SymH $ sortDesc xs)
        | otherwise = error "symH: not a partition"

instance (Eq k, Num k) => Algebra k SymH where
    unit x = x *> return (SymH [])
    mult = linear (\(SymH lambda, SymH mu) -> return $ SymH $ multisetSumDesc lambda mu)

instance (Eq k, Num k) => Coalgebra k SymH where
    counit = unwrap . linear counit' where counit' (SymH lambda) = if null lambda then 1 else 0
    comult = linear comult' where
        comult' (SymH [n]) = sumv [return (h i, h (n-i)) | i <- [0..n] ]
        comult' (SymH lambda) = product [comult' (SymH [n]) | n <- lambda]
        h 0 = SymH []
        h i = SymH [i]

instance (Eq k, Num k) => Bialgebra k SymH where {}

-- |Convert from the complete to the monomial basis of Sym
symHtoM :: (Eq k, Num k) => Vect k SymH -> Vect k SymM
symHtoM = linear symHtoM' where
    symHtoM' (SymH [n]) = sumv [return (SymM mu) | mu <- integerPartitions n]
    symHtoM' (SymH lambda) = product [symHtoM' (SymH [p]) | p <- lambda]


-- NSYM, THE HOPF ALGEBRA OF NON-COMMUTATIVE SYMMETRIC FUNCTIONS

-- |A basis for NSym, the Hopf algebra of non-commutative symmetric functions, indexed by compositions
newtype NSym = NSym [Int] deriving (Eq,Ord,Show)

nsym :: [Int] -> Vect Q NSym
nsym xs = return (NSym xs)
nsym xs | all (>0) xs = return (NSym xs)
        | otherwise = error "nsym: not a composition"

instance (Eq k, Num k) => Algebra k NSym where
    unit x = x *> return (NSym [])
    mult = linear mult' where
        mult' (NSym xs, NSym ys) = return $ NSym $ xs ++ ys

instance (Eq k, Num k) => Coalgebra k NSym where
    counit = unwrap . linear counit' where counit' (NSym zs) = if null zs then 1 else 0
    comult = linear comult' where
        comult' (NSym [n]) = sumv [return (z i, z (n-i)) | i <- [0..n] ]
        comult' (NSym zs) = product [comult' (NSym [n]) | n <- zs]
        z 0 = NSym []
        z i = NSym [i]

instance (Eq k, Num k) => Bialgebra k NSym where {}

-- Hazewinkel et al p233
instance (Eq k, Num k) => HopfAlgebra k NSym where
    antipode = linear antipode' where
        antipode' (NSym alpha) = sumv [(-1)^length beta *> return (NSym beta) | beta <- refinements (reverse alpha)]



-- MAPS BETWEEN (POSETS AND) HOPF ALGEBRAS

-- A descending tree is one in which a child is always less than a parent.
descendingTree [] = E
descendingTree [x] = T E x E
descendingTree xs = T l x r
    where x = maximum xs
          (ls,_:rs) = L.break (== x) xs
          l = descendingTree ls
          r = descendingTree rs
-- This is a bijection from permutations to "ordered trees".
-- It is order-preserving on trees with the same nodecount.
-- We can recover the permutation by reading the node labels in infix order.
-- This is the map called lambda in Loday.pdf


-- |Given a permutation p of [1..n], we can construct a tree (the descending tree of p) as follows:
--
-- * Split the permutation as p = ls ++ [n] ++ rs
--
-- * Place n at the root of the tree, and recursively place the descending trees of ls and rs as the left and right children of the root
--
-- * To bottom out the recursion, the descending tree of the empty permutation is of course the empty tree
--
-- This map between bases SSymF -> YSymF turns out to induce a morphism of Hopf algebras.
descendingTreeMap :: (Eq k, Num k) => Vect k SSymF -> Vect k (YSymF ())
descendingTreeMap = nf . fmap (YSymF . shape .  descendingTree')
    where descendingTree' (SSymF xs) = descendingTree xs
-- This is the map called Lambda in Loday.pdf, or tau in MSym.pdf
-- It is an algebra morphism.

-- One of the ideas in the MSym paper is to look at the intermediate result (fmap descendingTree' x),
-- which is an "ordered tree", and consider the map as factored through this

-- The map is surjective but not injective. The fibers tau^-1(t) are intervals in the weak order on permutations

-- "inverse" for descendingTree
-- These are the maps called gamma in Loday.pdf
minPerm t = minPerm' (lrCountTree t)
    where minPerm' E = []
          minPerm' (T l (lc,rc) r) = minPerm' l ++ [lc+rc+1] ++ map (+lc) (minPerm' r)

maxPerm t = maxPerm' (lrCountTree t)
    where maxPerm' E = []
          maxPerm' (T l (lc,rc) r) = map (+rc) (maxPerm' l) ++ [lc+rc+1] ++ maxPerm' r


-- The composition of [1..n] obtained by treating each left-facing leaf as a cut
-- Specifically, we visit the nodes in infix order, cutting after a node if it does not have an E as its right child
-- This is the map called L in Loday.pdf
leftLeafComposition E = []
leftLeafComposition t = cuts $ tail $ leftLeafs t
    where leftLeafs (T l x E) = leftLeafs l ++ [False]
          leftLeafs (T l x r) = leftLeafs l ++ leftLeafs r
          leftLeafs E = [True]
          cuts bs = case break id bs of
                    (ls,r:rs) -> (length ls + 1) : cuts rs
                    (ls,[]) -> [length ls]

leftLeafComposition' (YSymF t) = QSymF (leftLeafComposition t)

-- |A Hopf algebra morphism from YSymF to QSymF
leftLeafCompositionMap :: (Eq k, Num k) => Vect k (YSymF a) -> Vect k QSymF
leftLeafCompositionMap = nf . fmap leftLeafComposition'


-- The descent set of a permutation is [i | x_i > x_i+1], where we start the indexing from 1
descents [] = []
descents xs = map (+1) $ L.elemIndices True $ zipWith (>) xs (tail xs)

-- The composition of [1..n] obtained by treating each descent as a cut
descentComposition [] = []
descentComposition xs = dc $ zipWith (>) xs (tail xs) ++ [False]
    where dc bs = case break id bs of
                  (ls,r:rs) -> (length ls + 1) : dc rs
                  (ls,[]) -> [length ls]

-- |Given a permutation of [1..n], its descents are those positions where the next number is less than the previous number.
-- For example, the permutation [2,3,5,1,6,4] has descents from 5 to 1 and from 6 to 4. The descents can be regarded as cutting
-- the permutation sequence into segments - 235-16-4 - and by counting the lengths of the segments, we get a composition 3+2+1.
-- This map between bases SSymF -> QSymF turns out to induce a morphism of Hopf algebras.
descentMap :: (Eq k, Num k) => Vect k SSymF -> Vect k QSymF
descentMap = nf . fmap (\(SSymF xs) -> QSymF (descentComposition xs))
-- descentMap == leftLeafCompositionMap . descendingTreeMap

underComposition (QSymF ps) = foldr under (SSymF []) [SSymF [1..p] | p <- ps]
    where under (SSymF xs) (SSymF ys) = let q = length ys
                                            zs = map (+q) xs ++ ys -- so it has a global descent at the split
                                        in SSymF zs
-- This is a poset morphism (indeed, it forms a Galois connection with descentComposition)
-- but it does not extend to a Hopf algebra morphism.
-- (It does extend to a coalgebra morphism.)
-- (It is picking the maximum permutation having a given descent composition,
-- so there's an element of arbitrariness to it.)
-- This is the map called Z (Zeta?) in Loday.pdf

{-
-- This is O(n^2), whereas an O(n) implementation should be possible
-- Also, we would really like the associated composition (obtained by treating each global descent as a cut)?
globalDescents xs = globalDescents' 0 [] xs
    where globalDescents' i ls (r:rs) = (if minimum (infinity:ls) > maximum (0:r:rs) then [i] else [])
                                     ++ globalDescents' (i+1) (r:ls) rs
          globalDescents' n _ [] = [n]
          infinity = maxBound :: Int
-- The idea is that this leads to a map from SSymM to QSymM

globalDescentComposition [] = []
globalDescentComposition (x:xs) = globalDescents' 1 x xs
    where globalDescents' i minl (r:rs) = if minl > maximum (r:rs)
                                          then i : globalDescents' 1 r rs
                                          else globalDescents' (i+1) r rs
          globalDescents' i _ [] = [i]

globalDescentMap :: (Eq k, Num k) => Vect k SSymM -> Vect k QSymM
globalDescentMap = nf . fmap (\(SSymM xs) -> QSymM (globalDescentComposition xs))
-}

-- A multiplication operation on trees
-- (Connected with their being cofree)
-- (intended to be used as infix)
under E t = t
under (T l x r) t = T l x (under r t)

isUnderIrreducible (T l x E) = True
isUnderIrreducible _ = False

underDecomposition (T l x r) = T l x E : underDecomposition r
underDecomposition E = []


-- GHC7.4.1 doesn't like the following type signature - a bug.
-- ysymmToSh :: (Eq k, Num k) => Vect k (YSymM) => Vect k (Shuffle (PBT ()))
ysymmToSh = fmap ysymmToSh'
    where ysymmToSh' (YSymM t) = Sh (underDecomposition t)
-- This is a coalgebra morphism (but not an algebra morphism)
-- It shows that YSym is co-free
{-
-- This one not working yet - perhaps it needs an nf, or to go via S/YSymF, or ...
ssymmToSh = nf . fmap ssymmToSh'
    where ssymmToSh' (SSymM xs) = (Sh . underDecomposition . shape . descendingTree) xs
-}

-- |The injection of Sym into QSym (defined over the monomial basis)
symToQSymM :: (Eq k, Num k) => Vect k SymM -> Vect k QSymM
symToQSymM = linear symToQSymM' where
    symToQSymM' (SymM ps) = sumv [return (QSymM c) | c <- compositionsFromPartition ps]

-- We could equally well send NSym -> SymE, since the algebra and coalgebra definitions for SymE and SymH are exactly analogous.
-- However, NSym -> SymH is more natural, since it is consistent with the duality pairings below.
-- eg Hazewinkel 238ff
-- (Why do SymE and SymH have the same definitions? They're not dual bases. It's because of the Wronski relations.)
-- |A surjection of NSym onto Sym (defined over the complete basis)
nsymToSymH :: (Eq k, Num k) => Vect k NSym -> Vect k SymH
nsymToSymH = linear nsymToSym' where
    nsymToSym' (NSym zs) = return (SymH $ sortDesc zs)

-- The Hopf algebra morphism NSym -> Sym factors through NSym -> SSym -> YSym -> Sym (contained in QSym)
-- (?? This map NSym -> SSym is the dual of the descent map SSym -> QSym ??)
-- (Loday.pdf, p30)
-- (See also Hazewinkel p267-9)
nsymToSSym = linear nsymToSSym' where
    nsymToSSym' (NSym xs) = product [return (SSymF [1..n]) | n <- xs]


-- |A duality pairing between the complete and monomial bases of Sym, showing that Sym is self-dual.
instance (Eq k, Num k) => HasPairing k SymH SymM where
    pairing = linear pairing' where
        pairing' (SymH alpha, SymM beta) = delta alpha beta -- Kronecker delta
-- Hazewinkel p178
-- Actually to show duality you would need to show that the map SymH -> SymM*, v -> <v,.> is onto

-- |A duality pairing between NSym and QSymM (monomial basis), showing that NSym and QSym are dual.
instance (Eq k, Num k) => HasPairing k NSym QSymM where
    pairing = linear pairing' where
        pairing' (NSym alpha, QSymM beta) = delta alpha beta -- Kronecker delta
-- Hazewinkel p236-7
-- Actually to show duality you would need to show that the map NSym -> QSymM*, v -> <v,.> is onto

