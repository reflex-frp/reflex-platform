-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor #-}

-- |A module providing functions to construct and investigate (small, finite) matroids.
module Math.Combinatorics.Matroid where

-- Source: Oxley, Matroid Theory (second edition)

import Math.Core.Utils
import Math.Core.Field hiding (f7)

import Math.Common.ListSet as LS -- set operations on strictly ascending lists
-- import Math.Algebra.Field.Base hiding (Q, F2, F3, F5, F7, F11, f2, f3, f5, f7, f11)
import Math.Algebra.LinearAlgebra hiding (rank, (*>))

import qualified Math.Combinatorics.Graph as G -- hiding (combinationsOf, restriction, component, isConnected)
import Math.Combinatorics.FiniteGeometry
import Math.Combinatorics.GraphAuts
import Math.Algebra.Group.PermutationGroup hiding (closure)

import Math.Algebras.VectorSpace hiding (dual)
import Math.Algebras.Structures
import Math.Algebras.Commutative

import Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

implies p q = q || not p
exists = not . null
unique [x] = x

shortlex xs ys = case compare (length xs) (length ys) of
    LT -> LT
    EQ -> compare xs ys
    GT -> GT

isShortlex xs = foldcmpl (\x1 x2 -> shortlex x1 x2 /= GT) xs

toShortlex xs = map snd $ L.sort [(length x, x) | x <- xs]

isClutter ss = and [ (s1 `LS.isSubset` s2) `implies` (s1 == s2) | s1 <- ss, s2 <- ss ]
-- note that this definition would allow duplicates

-- single-element deletions of xs
-- if xs is sorted, then so is reverse (deletions xs)
deletions xs = zipWith (++) (inits xs) (tail $ tails xs)

closedUnderSubsets xss = and [xs' `S.member` xss' | xs <- xss, xs' <- deletions xs]
    where xss' = S.fromList xss


-- |The data structure that we use to store the bases of the matroid
data TrieSet a = TS [(a, TrieSet a)] deriving (Eq,Ord,Functor)
-- Note that in a trie we would normally have a Bool at each node saying whether or not the node is terminal
-- (ie corresponds to a member and not just a prefix of a member).
-- However, since we intend to use the trie to store the bases, and they are all the same length,
-- we can use lack of children to detect that a node is terminal.

-- We could try storing the children in a map rather than a list

-- for debugging
tsshow (TS xts) = "TS [" ++ concatMap (\(x,t) -> "(" ++ show x ++ "," ++ tsshow t ++ ")") xts ++ "]"

instance Show a => Show (TrieSet a) where
    show = show . tstolist

tsempty = TS []

tsinsert (x:xs) (TS ts) =
    case L.lookup x ts of
    Nothing -> let t = tsinsert xs (TS [])
               in TS $ L.insert (x,t) ts
    Just t -> let t' = tsinsert xs t
              in TS $ L.insert (x,t') $ L.delete (x,t) ts
tsinsert [] t = t

tsmember (x:xs) (TS ts) =
    case lookup x ts of
    Nothing -> False
    Just t -> tsmember xs t
tsmember [] (TS []) = True -- the node has no children, and hence is terminal
tsmember [] _ = False

-- xs is a subset of a member of t
tssubmember (x:xs) (TS ts) = or [ case compare x y of
                                  LT -> False
                                  EQ -> tssubmember xs t
                                  GT -> tssubmember (x:xs) t
                                | (y,t) <- ts ]
tssubmember [] _ = True

tstolist (TS []) = [[]]
tstolist (TS xts) = concatMap (\(x,t) -> map (x:) (tstolist t)) xts 

tsfromlist = foldl' (flip tsinsert) tsempty


-- |A datatype to represent a matroid. @M es bs@ is the matroid whose elements are @es@, and whose bases are @bs@.
-- The normal form is for the @es@ to be in order, for each of the @bs@ individually to be in order.
-- (So the TrieSet should have the property that any path from the root to a leaf is strictly increasing).
data Matroid a = M [a] (TrieSet a) deriving (Eq,Show,Functor)

-- |Return the elements over which the matroid is defined.
elements :: Matroid t -> [t]
elements (M es bs) = es

-- |Return all the independent sets of a matroid, in shortlex order.
indeps :: (Ord a) => Matroid a -> [[a]]
indeps m = bfs [ ([],es) ]
    where es = elements m
          bfs ( (ls,rs) : nodes ) =
              let ls' = reverse ls in
              if isIndependent m ls'
              then ls' : bfs ( nodes ++ successors (ls,rs) )
              else bfs nodes
          bfs [] = []
          successors (ls,rs) = [ (r:ls, rs') | r:rs' <- L.tails rs ]

isIndependent :: (Ord a) => Matroid a -> [a] -> Bool
isIndependent (M es bs) xs = xs `tssubmember` bs

isDependent :: (Ord a) => Matroid a -> [a] -> Bool
isDependent m = not . isIndependent m

-- |Are these the independent sets of a matroid? (The sets must individually be ordered.)
isMatroidIndeps :: (Ord a) => [[a]] -> Bool
isMatroidIndeps is =
    [] `elem` is &&
    closedUnderSubsets is &&
    and [ (l1 < l2) `implies` exists [e | e <- i2 LS.\\ i1, L.insert e i1 `elem` is]
        | i1 <- is, let l1 = length i1, i2 <- is, let l2 = length i2 ]

-- |Construct a matroid from its elements and its independent sets.
fromIndeps :: (Ord a) => [a] -> [[a]] -> Matroid a
fromIndeps es is = fromBases es bs
    where bs = dfs [] [([],es)]
          dfs bs ( node@(ls,rs) : nodes ) =
              let succs = successors node
              in if null succs then dfs (ls:bs) nodes else dfs bs (succs ++ nodes)
          dfs ls [] = let r = length $ last ls -- we know that the first one we found is a true base
                      in map reverse $ filter (\b -> length b == r) ls
                      -- we might have had null succs simply because we ran out of vectors to add
          successors (ls,rs) = [ (r:ls, rs') | r:rs' <- L.tails rs, (r:ls) `S.member` is' ]
          is' = S.fromList $ map reverse is

-- this seems to be just slightly slower
fromIndeps1 es is = fromBases es bs
    where b = greedy [] es -- first find any basis
          greedy ls (r:rs) = if (r:ls) `S.member` ris'
                             then greedy (r:ls) rs
                             else greedy ls rs
          greedy ls [] = reverse ls
          ris' = S.fromList $ map reverse is
          bs = closure S.empty (S.singleton b) -- now find all other bases by passing to "neighbouring" bases
          closure interior boundary =
              if S.null boundary
              then S.toList interior
              else let interior' = interior `S.union` boundary
                       boundary' = S.fromList [ b' | b <- S.toList boundary,
                                                     x <- b, y <- es LS.\\ b,
                                                     let b' = L.insert y (L.delete x b),
                                                     b' `S.notMember` interior',
                                                     b' `S.member` is' ]
                   in closure interior' boundary'
          is' = S.fromList is
-- The basis properties imply that the set of all bases is connected under the "neighbour" relation


-- |Given a matrix, represented as a list of rows, number the columns [1..],
-- and construct the matroid whose independent sets correspond to those sets of columns which are linearly independent
-- (or in case there are repetitions, those multisets of columns which are sets, and which are linearly independent).
vectorMatroid :: (Eq k, Fractional k) => [[k]] -> Matroid Int
vectorMatroid = vectorMatroid' . L.transpose

-- |Given a list of vectors (or rows of a matrix), number the vectors (rows) [1..], and construct the matroid whose independent sets
-- correspond to those sets of vectors (rows) which are linearly independent
-- (or in case there are repetitions, those multisets which are sets, and which are linearly independent).
vectorMatroid' :: (Eq k, Fractional k) => [[k]] -> Matroid Int
vectorMatroid' vs = fromBases (map fst vs') bs
    where vs' = zip [1..] vs
          bs = dfs [] [([],[],vs')]
          dfs ls (r@(i,ref,es) : rs) =
              let succs = successors r in
              if null succs then dfs (i:ls) (succs ++ rs) else dfs ls (succs ++ rs)
          dfs ls [] = let r = length $ last ls -- we know that the first one we found is a true base
                      in map reverse $ filter (\b -> length b == r) ls
                      -- we might have had null succs simply because we ran out of vectors to add
          successors (i,ref,es) = [(i',ref',es') | (j,e):es' <- L.tails es,
                                                   not (inSpanRE ref e),
                                                   let ref' = rowEchelonForm (ref ++ [e]), -- is this really better than e:ref?
                                                   let i' = j : i ]

-- |Given the edges of an undirected graph, number the edges [1..], and construct the matroid whose independent sets
-- correspond to those sets of edges which contain no cycle. The bases therefore correspond to maximal forests within the graph.
-- The edge set is allowed to contain loops or parallel edges.
cycleMatroid :: (Ord a) => [[a]] -> Matroid Int
cycleMatroid es = fromBases (map fst es') bs
    where es' = zip [1..] es
          bs = dfs [] [([], M.empty, es')]
          dfs ls (r@(i,ref,es) : rs) =
              let succs = successors r
              in if null succs then dfs (i:ls) (succs ++ rs) else dfs ls (succs ++ rs)
          dfs ls [] = let r = length $ last ls -- we know that the first one we found is a true base
                      in map reverse $ filter (\b -> length b == r) ls
                      -- we might have had null succs simply because we ran out of edges to add
          successors (i, reps, (j,[u,v]):es' ) =
              if u == v
              then successors (i, reps, es')
              else case (M.lookup u reps, M.lookup v reps) of
                   (Nothing, Nothing) -> (j:i, M.insert u u $ M.insert v u reps, es') : successors (i, reps, es')
                                         -- neither of these vertices has been seen before, so add this edge as a new tree in the forest
                   (Just u', Nothing) -> (j:i, M.insert v u' reps, es') : successors (i, reps, es')
                                         -- we have seen u before but not v, so add v to the u-tree
                   (Nothing, Just v') -> (j:i, M.insert u v' reps, es') : successors (i, reps, es')
                                         -- we have seen v before but not u, so add u to the v-tree
                   (Just u', Just v') -> if u' == v'
                                         then successors (i,reps,es')
                                              -- u and v are already in the same tree, so adding this edge would make a cycle
                                         else (j:i, M.map (\w -> if w == v' then u' else w) reps, es') : successors (i, reps, es')
                                              -- u and v are in different trees, so join the trees together
          successors (_, _, []) = []

-- A version of cycle matroid that retains the original edges rather than relabeling with integers.
-- Not really valid if there are parallel edges (because they can't be distinguished in the output).
-- Required for "markedfcim" below.
cycleMatroid' es = fmap lookupEdge $ cycleMatroid es
    where table = M.fromList $ zip [1..] es
          lookupEdge = (M.!) table


-- |Given a matroid over an arbitrary type, relabel to obtain a matroid over the integers.
to1n :: (Ord a) => Matroid a -> Matroid Int
to1n m = fmap to1n' m
    where es = elements m
          table = M.fromList $ zip es [1..]
          to1n' = (M.!) table


-- ISOMORPHISMS AND AUTOMORPHISMS

incidenceGraphB m = G.G vs' es'
    where es = elements m
          bs = bases m
          vs' = map Left es ++ map Right bs
          es' = L.sort [ [Left e, Right b] | b <- bs, e <- b ]
-- incidence graph for the matroid considered as an incidence structure between elements and bases

incidenceGraphC m = G.G vs' es'
    where es = elements m
          cs = L.sort $ circuits m
          vs' = map Left es ++ map Right cs
          es' = L.sort [ [Left e, Right c] | c <- cs, e <- c ]
-- incidence graph for the matroid considered as an incidence structure between elements and circuits

incidenceGraphH m = G.G vs' es'
    where es = elements m
          hs = L.sort $ hyperplanes m
          vs' = map Left es ++ map Right hs
          es' = L.sort [ [Left e, Right h] | h <- hs, e <- h ]
-- incidence graph for the matroid considered as an incidence structure between elements and hyperplanes
-- for "sparse" matroids, there are likely to be fewer hyperplanes than bases (why?)
-- incidenceGraphH may not be connected - eg u 2 4

-- (So a rank 3 or higher matroid, provided it has more than one hyperplane, certainly has a connected incidenceGraphH)

matroidIsos m1 m2 = incidenceIsos (incidenceGraphH m1) (incidenceGraphH m2)

-- |Are the two matroids isomorphic?
isMatroidIso :: (Ord a, Ord b) => Matroid a -> Matroid b -> Bool
isMatroidIso m1 m2 = isIncidenceIso (incidenceGraphH m1) (incidenceGraphH m2)

-- |Return the automorphisms of the matroid.
matroidAuts :: (Ord a) => Matroid a -> [Permutation a]
matroidAuts m = incidenceAuts $ incidenceGraphH m
-- Note that the results aren't always what one intuitively expects from the geometric representation.
-- This is because geometric representations suggest additional structure beyond matroid structure.
-- For example, for the Vamos matroid v8,
-- it returns auts which are not "geometric" auts of the geometric representation.
-- Matroids are really combinatorial objects, not geometric.
-- In the case of v8, the key is that the only thing the auts have to preserve are the planes, not the lines


-- CIRCUITS

-- |A circuit in a matroid is a minimal dependent set.
isCircuit :: (Ord a) => Matroid a -> [a] -> Bool
isCircuit m c =
    isDependent m c &&
    all (isIndependent m) (deletions c)

-- |Return all circuits for the given matroid, in shortlex order.
circuits :: (Ord a) => Matroid a -> [[a]]
circuits m = toShortlex $ dfs S.empty [L.insert e b | b <- bs, e <- es LS.\\ b]
    where es = elements m
          bs = bases m
          dfs vs (c:cs) | c `S.member` vs = dfs vs cs
                        | otherwise = let cs' = successors c
                                          vs' = S.insert c vs
                                      in if null cs' then c : dfs vs' cs else dfs vs' (cs' ++ cs)
          dfs _ [] = []
          successors c = [c' | c' <- deletions c, isDependent m c' ]

-- Oxley p10
-- |Are the given sets the circuits of some matroid?
isMatroidCircuits :: (Ord a) => [[a]] -> Bool
isMatroidCircuits cs =
    [] `notElem` cs &&
    and [(c1 `LS.isSubset` c2) `implies` (c1 == c2) | c1 <- cs, c2 <- cs] &&
    and [ exists [c3 | c3 <- cs, c3 `LS.isSubset` c12']
        | c1 <- cs, c2 <- cs, c1 /= c2,
          e <- c1 `LS.intersect` c2, let c12' = L.delete e (c1 `LS.union` c2)] 

-- |Reconstruct a matroid from its elements and circuits.
fromCircuits :: (Ord a) => [a] -> [[a]] -> Matroid a
fromCircuits es cs = fromBases es bs
    where b = greedy [] es -- first find any basis
          greedy ls (r:rs) = let ls' = ls ++ [r] in
                             if isIndep ls'
                             then greedy ls' rs
                             else greedy ls rs
          greedy ls [] = ls
          bs = closure S.empty (S.singleton b) -- now find all other bases by passing to "neighbouring" bases
          closure interior boundary =
              if S.null boundary
              then S.toList interior
              else let interior' = interior `S.union` boundary
                       boundary' = S.fromList [ b' | b <- S.toList boundary,
                                                     x <- b, y <- es LS.\\ b,
                                                     let b' = L.insert y (L.delete x b),
                                                     b' `S.notMember` interior',
                                                     isIndep b' ]
                   in closure interior' boundary'
          isIndep xs = not (any (`LS.isSubset` xs) cs)


-- |An element e in a matroid M is a loop if {e} is a circuit of M.
isLoop :: (Ord a) => Matroid a -> a -> Bool
isLoop m e = isCircuit m [e]
-- isLoop (M es is) e = [e] `notElem` is

-- |Elements f and g in a matroid M are parallel if {f,g} is a circuit of M.
isParallel :: (Ord a) => Matroid a -> a -> a -> Bool
isParallel m f g = isCircuit m [f,g]

-- |A matroid is simple if it has no loops or parallel elements
isSimple :: (Ord a) => Matroid a -> Bool
isSimple m = all ( (>2) . length ) (circuits m)


-- BASES

-- |A base or basis in a matroid is a maximal independent set.
isBase :: (Ord a) => Matroid a -> [a] -> Bool
isBase (M es bs) b = b `tsmember` bs

-- |Return all bases for the given matroid
bases :: (Ord a) => Matroid a -> [[a]]
bases (M es bs) = tstolist bs

-- |Are the given sets the bases of some matroid?
isMatroidBases :: (Ord a) => [[a]] -> Bool
isMatroidBases bs =
    (not . null) bs &&
    and [ exists [y | y <- b2 LS.\\ b1, L.insert y (L.delete x b1) `elem` bs]
        | b1 <- bs, b2 <- bs, x <- b1 LS.\\ b2 ]

-- |Reconstruct a matroid from its elements and bases.
fromBases :: (Ord a) => [a] -> [[a]] -> Matroid a
fromBases es bs = M es (tsfromlist bs)
-- The elements are required because a loop does not appear in any basis.

-- Oxley p17
-- |Given a matroid m, a basis b, and an element e, @fundamentalCircuit m b e@ returns the unique circuit contained in b union {e},
-- which is called the fundamental circuit of e with respect to b.
fundamentalCircuit :: (Ord a) => Matroid a -> [a] -> a -> [a]
fundamentalCircuit m b e = unique [c | c <- circuits m, c `LS.isSubset` be]
    where be = L.insert e b

uniformMatroid m n | m <= n = fromBases es bs
    where es = [1..n]
          bs = combinationsOf m es

-- |The uniform matroid U m n is the matroid whose independent sets are all subsets of [1..n] with m or fewer elements.
u :: Int -> Int -> Matroid Int
u = uniformMatroid


-- RANK FUNCTION

-- Oxley p103, 3.1.14
restriction1 m xs = fromBases xs bs'
    where bs = bases m
          is' = toShortlex [b `LS.intersect` xs | b <- bs]
          r = length $ last is'
          bs' = dropWhile ( (< r) . length ) is'

-- |The restriction of a matroid to a subset of its elements
restriction :: (Ord a) => Matroid a -> [a] -> Matroid a
restriction m@(M es bs) xs = M xs bs'
    where (_,bs') = balance $ prune bs
          prune (TS yts) = let (ins, outs) = L.partition (\(y,t) -> y `elem` xs) yts
                               ins' = [(y, prune t) | (y,t) <- ins]
                               outs' = concat [ zts | (y,t) <- outs, let TS zts = prune t ]
                           in TS $ ins' ++ outs'
          balance (TS yts) = let dyt's = [(d',(y,t')) | (y,t) <- yts, let (d',t') = balance t]
                                 d = maximum $ 0 : map fst dyt's
                             in (d+1, TS $ toSet [(y,t') | (d',(y,t')) <- dyt's, d' == d])
-- we have to make the toSet call *after* we balance, otherwise two trees may appear unequal because of undergrowth that is going to be removed

-- !! Need thorough testing to prove that restriction == restriction1


-- |Given a matroid m, @rankfun m@ is the rank function on subsets of its element set
rankfun :: (Ord a) => Matroid a -> [a] -> Int
rankfun m xs = (length . head . bases) (restriction m xs)
-- no danger of head [], because bases must be non-null

-- |The rank of a matroid is the cardinality of a basis
rank :: (Ord a) => Matroid a -> Int
rank m = length $ head $ bases m
-- rank m@(M es bs) = rankfun m es

-- Oxley p23
-- |Reconstruct a matroid from its elements and rank function
fromRankfun :: (Ord a) => [a] -> ([a] -> Int) -> Matroid a
fromRankfun es rkf = fromBases es bs
    where b = greedy 0 [] es -- first find any basis
          greedy rk ls (r:rs) = let ls' = ls ++ [r] in
                                if rkf ls' == rk+1
                                then greedy (rk+1) ls' rs
                                else greedy rk ls rs
          greedy _ ls [] = ls
          rk = rkf b
          isBasis b' = rkf b' == rk
          bs = closure S.empty (S.singleton b) S.empty -- now find all other bases by passing to "neighbouring" bases
          closure interior boundary exterior =
              if S.null boundary
              then S.toList interior
              else let interior' = interior `S.union` boundary
                       candidates = S.fromList [ b' | b <- S.toList boundary,
                                                      x <- b, y <- es LS.\\ b,
                                                      let b' = L.insert y (L.delete x b),
                                                      b' `S.notMember` interior',
                                                      b' `S.notMember` exterior ]
                       (boundary', exterior') = S.partition isBasis candidates
                   in closure interior' boundary' (S.union exterior exterior')
-- The purpose of keeping track of exterior points we have encountered
-- is to avoid making repeat calls to the rank function rkf


-- CLOSURE OPERATOR AND FLATS

-- |Given a matroid m, @closure m@ is the closure operator on subsets of its element set
closure :: (Ord a) => Matroid a -> [a] -> [a]
closure m xs = [x | x <- es, x `elem` xs || rankfun m (L.insert x xs) == rankxs]
    where es = elements m
          rankxs = rankfun m xs
-- The intuition is that closure xs is all elements within the span of xs

-- |Reconstruct a matroid from its elements and closure operator
fromClosure :: (Ord a) => [a] -> ([a] -> [a]) -> Matroid a
fromClosure es cl = fromBases es bs
    where b = greedy (cl []) [] es -- first find any basis
          greedy span ls (r:rs) = let ls' = ls ++ [r] in
                                  if r `notElem` span -- r is independent relative to ls
                                  then greedy (cl ls') ls' rs
                                  else greedy span ls rs
          greedy _ ls [] = ls
          rk = length b
          isBasis b' = cl b' == es
          bs = closure S.empty (S.singleton b) S.empty -- now find all other bases by passing to "neighbouring" bases
          closure interior boundary exterior =
              if S.null boundary
              then S.toList interior
              else let interior' = interior `S.union` boundary
                       candidates = S.fromList [ b' | b <- S.toList boundary,
                                                      x <- b, y <- es LS.\\ b,
                                                      let b' = L.insert y (L.delete x b),
                                                      b' `S.notMember` interior',
                                                      b' `S.notMember` exterior ]
                       (boundary', exterior') = S.partition isBasis candidates
                   in closure interior' boundary' (S.union exterior exterior')
-- The purpose of keeping track of exterior points we have encountered
-- is to avoid making repeat calls to the closure operator cl

{-
-- Not quite sure why the following is so much slower than the above
-- May just be because Data.Set is compiled, and this would be comparable if also compiled
fromClosure2 es cl = M es bs
    where b = greedy (cl []) [] es -- first find any basis
          greedy span ls (r:rs) = let ls' = ls ++ [r] in
                                  if r `notElem` span -- r is independent relative to ls
                                  then greedy (cl ls') ls' rs
                                  else greedy span ls rs
          greedy _ ls [] = ls
          rk = length b
          isBasis b' = cl b' == es
          bs = closure tsempty [b] tsempty -- now find all other bases by passing to "neighbouring" bases
          closure interior boundary exterior =
              if null boundary
              then interior
              else let interior' = foldl' (flip tsinsert) interior boundary
                       candidates = [ b' | b <- boundary,
                                           x <- b, y <- es LS.\\ b,
                                           let b' = L.insert y (L.delete x b),
                                           not (b' `tsmember` interior'),
                                           not (b' `tsmember` exterior) ]
                       (boundary', exterior') = L.partition isBasis candidates
                   in closure interior' boundary' (foldl' (flip tsinsert) exterior exterior')
-}

-- |A flat in a matroid is a closed set, that is a set which is equal to its own closure
isFlat :: (Ord a) => Matroid a -> [a] -> Bool
isFlat m xs = closure m xs == xs

flats1 m = [xs | xs <- powersetbfs es, isFlat m xs]
    where es = elements m
-- first, inefficient, implementation

-- given xs, a flat in m, return the flats which cover xs
-- these have the property that they partition es \\ xs
coveringFlats m xs = coveringFlats' (es LS.\\ xs)
    where es = elements m
          coveringFlats' (y:ys) = let zs = closure m (L.insert y xs)
                                  in zs : coveringFlats' (ys LS.\\ zs)
          coveringFlats' [] = []

-- since we are dealing with finite matroids, the lattice of flats is finite, so it has a minimal element
minimalFlat m = head $ filter (isFlat m) $ powersetbfs $ elements m

-- |The flats of a matroid are its closed sets. They form a lattice under inclusion.
flats :: (Ord a) => Matroid a -> [[a]]
flats m = flats' S.empty [minimalFlat m]
    where flats' ls (r:rs) = if r `S.member` ls
                             then flats' ls rs
                             else flats' (S.insert r ls) (rs ++ coveringFlats m r)
          flats' ls [] = toShortlex $ S.toList ls
          -- this is just breadth-first search

-- isMatroidFlats
-- Oxley p31-32
{-
isMatroidFlats es fs =
    es `elem` fs &&
    [ (f1 `LS.intersect` f2) `elem` fs | f1 <- fs, f2 <- fs ] &&
    -- for all flats f, the minimal flats containing f partition es-f
-}

-- |Reconstruct a matroid from its flats. (The flats must be given in shortlex order.)
fromFlats :: (Ord a) => [[a]] -> Matroid a
fromFlats fs | isShortlex fs = fromFlats' fs
             | otherwise = error "fromFlats: flats must be in shortlex order"

fromFlats' fs = fromClosure es cl
    where es = last fs -- es is a flat, and last in shortlex order
          cl xs = head [f | f <- fs, xs `LS.isSubset` f] -- the first flat is minimal, because of shortlex order
-- !! we can probably do better (efficiency-wise)
-- eg by constructing the lattice as a DAG, and climbing up it

-- |A subset of the elements in a matroid is spanning if its closure is all the elements
isSpanning :: (Ord a) => Matroid a -> [a] -> Bool
isSpanning m xs = closure m xs == es
    where es = elements m

-- |A hyperplane is a flat whose rank is one less than that of the matroid
isHyperplane :: (Ord a) => Matroid a -> [a] -> Bool
isHyperplane m xs = isFlat m xs && rankfun m xs == rank m - 1

hyperplanes1 m = [h | h <- flats m, rankfun m h == rk - 1]
    where rk = rank m

-- Oxley p65: h is a hyperplane iff its complement is a cocircuit
hyperplanes :: (Ord a) => Matroid a -> [[a]]
hyperplanes m = toShortlex $ map complement $ cocircuits m
    where es = elements m
          complement cc = es LS.\\ cc
-- This appears to be faster than hyperplanes1

-- Oxley p70
isMatroidHyperplanes :: (Ord a) => [a] -> [[a]] -> Bool
isMatroidHyperplanes es hs =
    es `notElem` hs &&
    isClutter hs &&
    and [ exists [h3 | h3 <- hs, h12e `LS.isSubset` h3] | (h1,h2) <- pairs hs,
                                                          e <- es LS.\\ (LS.union h1 h2),
                                                          let h12e = L.insert e (LS.intersect h1 h2) ]
-- Note that contrary to what one might initially think
-- it does *not* follow from the third condition that every element is in some hyperplane
-- since there might be only one hyperplane, eg fromBases [1,2] [[2]] has [1] as it's only hyperplane

-- Haven't actually proven that the following is always correct
-- but it seems to work
fromHyperplanes1 es hs = fromFlats $ closure S.empty (S.fromList hs)
    where closure interior boundary =
              if S.null boundary
              then (toShortlex $ S.toList interior) ++ [es]
              else let interior' = S.union interior boundary
                       boundary' = S.fromList [ f1 `LS.intersect` f2 | (f1,f2) <- pairs (S.toList boundary) ]
                                   S.\\ interior'
                   in closure interior' boundary'

-- |Reconstruct a matroid from its elements and hyperplanes
fromHyperplanes :: (Ord a) => [a] -> [[a]] -> Matroid a
fromHyperplanes es hs = fromCocircuits es $ map complement hs
    where fromCocircuits es = dual . fromCircuits es
          complement xs = es LS.\\ xs


-- GEOMETRIC REPRESENTATION

-- |Given a list of points in k^n, number the points [1..], and construct the matroid whose independent sets
-- correspond to those sets of points which are affinely independent.
--
-- A multiset of points in k^n is said to be affinely dependent if it contains two identical points,
-- or three collinear points, or four coplanar points, or ... - and affinely independent otherwise.
affineMatroid :: (Eq k, Fractional k) => [[k]] -> Matroid Int
affineMatroid vs = vectorMatroid' $ map (1:) vs

-- |fromGeoRep returns a matroid from a geometric representation consisting of dependent flats of various ranks.
-- Given lists of dependent rank 0 flats (loops), rank 1 flats (points), rank 2 flats (lines) and rank 3 flats (planes),
-- @fromGeoRep loops points lines planes@ returns the matroid having these as dependent flats.
-- Note that if all the elements lie in the same plane, then this should still be listed as an argument.
fromGeoRep :: (Ord a) => [[a]] -> [[a]] -> [[a]] -> [[a]] -> Matroid a
fromGeoRep loops points lines planes =
    fromCircuits es $ minimal $
        loops ++
        concatMap (combinationsOf 2) points ++
        concatMap (combinationsOf 3) lines ++
        concatMap (combinationsOf 4) planes ++
        combinationsOf 5 es
    where es = toSet $ concat loops ++ concat points ++ concat lines ++ concat planes

-- Note that we don't check that the inputs are valid

-- xss assumed to be in shortlex order
minimal xss = minimal' [] xss
    where minimal' ls (r:rs) = if any (`LS.isSubset` r) ls
                               then minimal' ls rs
                               else minimal' (r:ls) rs
          minimal' ls [] = reverse ls

-- |A simple matroid has no loops or parallel elements, hence its geometric representation has no loops or dependent points.
-- @simpleFromGeoRep lines planes@ returns the simple matroid having these dependent flats.
simpleFromGeoRep :: (Ord a) => [[a]] -> [[a]] -> Matroid a
simpleFromGeoRep lines planes = fromGeoRep [] []  lines planes

-- Oxley p37-8
isSimpleGeoRep lines planes =
    all ( (<= 1) . length ) [ l1 `LS.intersect` l2 | (l1,l2) <- pairs lines ] &&
    all ( \i -> length i <= 2 || i `elem` lines ) [ p1 `LS.intersect` p2 | (p1,p2) <- pairs planes ] &&
    and [ any (u `LS.isSubset`) planes | (l1,l2) <- pairs lines, length (l1 `LS.intersect` l2) == 1, let u = l1 `LS.union` l2 ] &&
    and [ length i == 1 || i == l | l <- lines, p <- planes, let i = l `LS.intersect` p ]


isCircuitHyperplane m xs = isCircuit m xs && isHyperplane m xs

-- |List the circuit-hyperplanes of a matroid.
circuitHyperplanes :: (Ord a) => Matroid a -> [[a]]
circuitHyperplanes m = [ h | h <- hyperplanes m, isCircuit m h ] 

-- Oxley p39
-- |Given a matroid m, and a set of elements b which is both a circuit and a hyperplane in m,
-- then @relaxation m b@ is the matroid which is obtained by adding b as a new basis.
-- This corresponds to removing b from the geometric representation of m.
relaxation :: (Ord a) => Matroid a -> [a] -> Matroid a
relaxation m b
    | isCircuitHyperplane m b = fromBases es bs
    | otherwise = error "relaxation: not a circuit-hyperplane"
    where es = elements m
          bs = b : bases m 


-- TRANSVERSAL MATROIDS

ex161 = [ [1,2,6], [3,4,5,6], [2,3], [2,4,6] ]

-- the edges of the bipartite graph
transversalGraph as = [(Left x, Right i) | (a,i) <- zip as [1..], x <- a]

partialMatchings es = dfs [(S.empty, [], es)]
    where dfs (node@(vs,ls,rs): nodes) = ls : dfs (successors node ++ nodes)
          dfs [] = []
          successors (vs,ls,rs) =  [ (S.insert u $ S.insert v vs, L.insert r ls, rs')
                                   | r@(u,v):rs' <- L.tails rs, u `S.notMember` vs, v `S.notMember` vs ]

-- |Given a set of elements es, and a sequence as = [a1,...,am] of subsets of es,
-- return the matroid whose independent sets are the partial transversals of the as.
transversalMatroid :: (Ord a) => [a] -> [[a]] -> Matroid a
transversalMatroid es as = fromBases es bs
    where is@(i:_) = reverse $ toShortlex $ toSet $ (map . map) (unLeft . fst) $ partialMatchings (transversalGraph as)
          unLeft (Left x) = x
          l = length i
          bs = reverse $ takeWhile ( (== l) . length ) is
-- In this case, as is called a presentation of the matroid
-- Note that there may be partial transversals even if there are no transversals


-- Not obvious how to efficiently find the bases without finding the independent sets,
-- since neighbouring partial transversals might arise in different ways
-- (Although Oxley p93 seems to be saying that they don't)
{-
transversalMatroid2 es as = fromBases es bs
    where es' = transversalGraph as
          b = greedy [] es' -- first find any basis
          greedy ls (r@(u,v):rs) = if u `notElem` map fst ls && v `notElem` map snd ls
                                   then greedy (r:ls) rs
                                   else greedy ls rs
          greedy ls [] = ls
-- now choose the subset of the as indicated by the Right part of b
-- and seek all full transversals
          bs = closure S.empty (S.singleton b) -- now find all other bases by passing to "neighbouring" bases
          closure interior boundary =
              if S.null boundary
              then S.toList interior
              else let interior' = interior `S.union` boundary
                       boundary' = S.fromList [ b' | b <- S.toList boundary,
                                                     x <- b, y <- es LS.\\ b,
                                                     let b' = L.insert y (L.delete x b),
                                                     b' `S.notMember` interior',
                                                     cl b' == es ] -- b' is spanning, and same size as b, hence must be basis
                                   -- S.\\ interior'
                   in closure interior' boundary'
-}


-- DUALITY

-- |The dual matroid
dual :: (Ord a) => Matroid a -> Matroid a
dual m = fromBases es bs'
    where es = elements m
          bs = bases m
          bs' = L.sort $ map (es LS.\\) bs

isCoindependent m xs = isIndependent (dual m) xs

isCobase m xs = isBase (dual m) xs
-- quicker but less clear to calculate this directly

isCocircuit m xs = isCircuit (dual m) xs

cocircuits :: (Ord a) => Matroid a -> [[a]]
cocircuits m = circuits (dual m)

isColoop m e = isLoop (dual m) e

isCoparallel m f g = isParallel (dual m) f g


-- MINORS

deletion :: (Ord a) => Matroid a -> [a] -> Matroid a
deletion m xs = restriction m (es LS.\\ xs)
    where es = elements m

(\\\) = deletion

contraction :: (Ord a) => Matroid a -> [a] -> Matroid a
contraction m xs = dual (deletion (dual m) xs)

(///) = contraction


-- CONNECTIVITY

-- Oxley p120
-- |A matroid is (2-)connected if, for every pair of distinct elements, there is a circuit containing both
isConnected :: (Ord a) => Matroid a -> Bool
isConnected m = and [any (pair `LS.isSubset`) cs | pair <- combinationsOf 2 es]
    where es = elements m
          cs = circuits m

component m x = closure S.empty (S.singleton x)
    where cs = circuits m
          closure interior boundary =
              if S.null boundary
              then S.toList interior
              else let interior' = S.union interior boundary
                       boundary' = S.fromList (concat [c | c <- cs, (not . null) (LS.intersect c (S.toList boundary)) ])
                                   S.\\ interior'
                   in closure interior' boundary'

-- |The direct sum of two matroids
dsum :: (Ord a, Ord b) => Matroid a -> Matroid b -> Matroid (Either a b)
dsum m1 m2 = fromBases es bs
    where es = map Left (elements m1) ++ map Right (elements m2)
          bs = [map Left b1 ++ map Right b2 | b1 <- bases m1, b2 <- bases m2]


-- REPRESENTABILITY

-- |@matroidPG n fq@ returns the projective geometry PG(n,Fq), where fq is a list of the elements of Fq
matroidPG :: (Eq a, Fractional a) => Int -> [a] -> Matroid Int
matroidPG n fq = vectorMatroid' $ ptsPG n fq

-- |@matroidAG n fq@ returns the affine geometry AG(n,Fq), where fq is a list of the elements of Fq
matroidAG :: (Eq a, Fractional a) => Int -> [a] -> Matroid Int
matroidAG n fq = vectorMatroid' $ ptsAG n fq


-- Oxley p182
-- |Given a matroid m, the fundamental-circuit incidence matrix relative to a base b
-- has rows indexed by the elements of b, and columns indexed by the elements not in b.
-- The bi, ej entry is 1 if bi is in the fundamental circuit of ej relative to b, and 0 otherwise.
fundamentalCircuitIncidenceMatrix :: (Ord a, Num k) => Matroid a -> [a] -> [[k]]
fundamentalCircuitIncidenceMatrix m b = L.transpose $ fundamentalCircuitIncidenceMatrix' m b

fundamentalCircuitIncidenceMatrix' m b =
    [ [if e `elem` fundamentalCircuit m b e' then 1 else 0 | e <- b]
    | e' <- elements m LS.\\ b ]

fcim = fundamentalCircuitIncidenceMatrix
fcim' = fundamentalCircuitIncidenceMatrix'

-- Then fcim w4 [1..4] == L.transpose (fcim (dual w4) [5..8])


-- Given a matrix of 0s and 1s, return a matrix of 0s, 1s and *s
-- where 0 -> 0, and 1 -> 1 if it is the first 1 in either a row or column, 1 -> * otherwise
markNonInitialRCs mx = mark (replicate w False) mx
    where w = length $ head mx -- the width
          mark cms (r:rs) = let (cms', r') = mark' False [] cms [] r in r' : mark cms' rs
          mark _ [] = []
          mark' rm cms' (cm:cms) ys (x:xs)
              | x == 0 = mark' rm (cm:cms') cms (Zero:ys) xs
              | x == 1 = if rm && cm
                         then mark' True (True:cms') cms (Star:ys) xs
                         else mark' True (True:cms') cms (One:ys) xs
          mark' _ cms' [] ys [] = (reverse cms', reverse ys)

-- Given a matrix of 0s, 1s and *s, return all distinct matrices that can be obtained
-- by substituting non-zero elements of Fq for the *s
substStars mx fq = substStars' mx
    where fq' = tail fq -- non-zero elts of fq
          substStars' (r:rs) = [r':rs' | r' <- substStars'' r, rs' <- substStars' rs]
          substStars' [] = [[]]
          substStars'' (Zero:xs) = map (0:) $ substStars'' xs
          substStars'' (One:xs) = map (1:) $ substStars'' xs
          substStars'' (Star:xs) = [x':xs' | x' <- fq', xs' <- substStars'' xs]
          substStars'' [] = [[]]

starSubstitutionsV fq' (Zero:xs) = map (0:) $ starSubstitutionsV fq' xs
starSubstitutionsV fq' (One:xs) = map (1:) $ starSubstitutionsV fq' xs
starSubstitutionsV fq' (Star:xs) = [x':xs' | x' <- fq', xs' <- starSubstitutionsV fq' xs]
starSubstitutionsV _ [] = [[]]


-- Oxley p184-5
-- Note that the particular representations you get depend on which basis is used
-- (Perhaps we should let you pass in the basis to use)
representations1 fq m = [ L.transpose d | d <- substStars dhash fq, let mx = ir ++ d,
                                          to1n m == (vectorMatroid' $ map snd $ L.sort $ zip (b ++ b') mx) ]
    where b = head $ bases m
          b' = elements m LS.\\ b
          r = length b -- rank of the matroid
          ir = idMx r -- identity matrix
          dhash = markNonInitialRCs $ fcim' m b

-- edges of the fundamental circuit incidence graph
fcig m b = [ [e,e'] | e <- b, e' <- elements m LS.\\ b, e `elem` fundamentalCircuit m b e' ]

-- the fcim of m relative to b, with 1s and *s marked
markedfcim m b = mark b b' (fcim m b)
    where b' = elements m LS.\\ b -- the elements of b are the row labels, those of b' are the column labels
          entries = fcig m b -- the [row,column] coordinates of the non-zero entries in the fcim
          ones = head $ bases $ cycleMatroid' entries -- a set of entries which we can take to be 1
          stars = entries LS.\\ ones -- the set of entries which we are then still free to assign
          mark (i:is) js (r:rs) = (mark' i js r) : mark is js rs
          mark [] _ [] = []
          mark' i (j:js) (x:xs)
              | x == 0 = Zero : mark' i js xs
              | x == 1 = (if [i,j] `elem` stars then Star else One) : mark' i js xs
          mark' _ [] [] = []
-- The markedfcim will sometimes do better than markNonInitialRCs, and is best possible

-- Oxley p184-5
representations2 fq m = [ L.transpose mx | d <- substStars dhash' fq, let mx = ir ++ d,
                                           m' == (vectorMatroid' $ map snd $ L.sort $ zip (b ++ b') mx) ]
    where m' = to1n m
          es = elements m
          b = head $ bases m
          b' = es LS.\\ b
          r = length b -- rank of the matroid
          ir = idMx r -- identity matrix
          dhash' = L.transpose $ markedfcim m b

-- In the following, we check the representations of the restrictions as we add a column at a time
-- This enables us to early out if a potential representation doesn't work on a restriction

-- |Find representations of the matroid m over fq. Specifically, this function will find one representative
-- of each projective equivalence class of representation.
representations :: (Eq fq, Fractional fq, Ord a) => [fq] -> Matroid a -> [[[fq]]]
representations fq m = map L.transpose $ representations' (reverse $ zip b ir) (zip b' dhash')
    where fq' = tail fq -- fq \ {0}
          b = head $ bases m
          b' = elements m LS.\\ b
          r = length b -- rank of the matroid
          ir = idMx r -- identity matrix
          dhash' = L.transpose $ markedfcim m b
          representations' ls ((i,r):rs) = concat
              [ representations' ((i,r'):ls) rs
              | r' <- starSubstitutionsV fq' r,
                let (is,vs) = unzip $ L.sortBy cmpfst ((i,r'):ls),
                to1n (restriction m is) == (vectorMatroid' vs) ]
          representations' ls [] = [map snd $ reverse ls]

-- |Is the matroid representable over Fq? For example, to find out whether a matroid m is binary, evaluate @isRepresentable f2 m@.
isRepresentable :: (Eq fq, Fractional fq, Ord a) => [fq] -> Matroid a -> Bool
isRepresentable fq m = (not . null) (representations fq m)

-- |A binary matroid is a matroid which is representable over F2
isBinary :: (Ord a) => Matroid a -> Bool
isBinary = isRepresentable f2

-- |A ternary matroid is a matroid which is representable over F3
isTernary :: (Ord a) => Matroid a -> Bool
isTernary = isRepresentable f3


-- CONSTRUCTIONS
-- The next three functions not very thoroughly tested

data LMR a b = L a | Mid | R b deriving (Eq, Ord, Show)

-- Oxley p252
seriesConnection (m1,p1) (m2,p2)
    | not (isLoop m1 p1) && not (isColoop m1 p1) && not (isLoop m2 p2) && not (isColoop m2 p2) =
        fromCircuits es cs
    | otherwise = error "not yet implemented"
        where es = map L (elements m1 LS.\\ [p1]) ++ [Mid] ++ map R (elements m2 LS.\\ [p2])
              cs = (map . map) L (circuits $ m1 \\\ [p1]) ++
                   (map . map) R (circuits $ m2 \\\ [p2]) ++
                   [ map L (L.delete p1 c1) ++ [Mid] ++ map R (L.delete p2 c2)
                   | c1 <- circuits m1, p1 `elem` c1, c2 <- circuits m2, p2 `elem` c2]

parallelConnection (m1,p1) (m2,p2)
    | not (isLoop m1 p1) && not (isColoop m1 p1) && not (isLoop m2 p2) && not (isColoop m2 p2) =
        fromCircuits es cs
    | otherwise = error "not yet implemented"
        where es = map L (elements m1 LS.\\ [p1]) ++ [Mid] ++ map R (elements m2 LS.\\ [p2])
              cs = (map . map) L (circuits $ m1 \\\ [p1]) ++
                   [ map L (L.delete p1 c1) ++ [Mid] | c1 <- circuits m1, p1 `elem` c1 ] ++
                   (map . map) R (circuits $ m2 \\\ [p2]) ++
                   [ [Mid] ++ map R (L.delete p2 c2) | c2 <- circuits m2, p2 `elem` c2 ] ++
                   [ map L (L.delete p1 c1) ++ map R (L.delete p2 c2)
                   | c1 <- circuits m1, p1 `elem` c1, c2 <- circuits m2, p2 `elem` c2 ]

twoSum (m1,p1) (m2,p2)
    | not (isLoop m1 p1) && not (isColoop m1 p1) && not (isLoop m2 p2) && not (isColoop m2 p2) =
        fromCircuits es cs
    | otherwise = error "not yet implemented"
        where es = map L (elements m1 LS.\\ [p1]) ++ [Mid] ++ map R (elements m2 LS.\\ [p2])
              cs = (map . map) L (circuits $ m1 \\\ [p1]) ++
                   (map . map) R (circuits $ m2 \\\ [p2]) ++
                   [ map L (L.delete p1 c1) ++ map R (L.delete p2 c2)
                   | c1 <- circuits m1, p1 `elem` c1, c2 <- circuits m2, p2 `elem` c2]
-- Note: The only different from seriesConnection is that we don't have Mid in the last set

-- Oxley p427
matroidUnion m1 m2 = fromBases es bs
    where es = LS.union (elements m1) (elements m2)
          is = toShortlex $ toSet [ LS.union b1 b2 | b1 <- bases m1, b2 <- bases m2 ]
          r = length $ last is
          bs = dropWhile ( (< r) . length ) is



-- SOME INTERESTING MATROIDS

-- |The Fano plane F7 = PG(2,F2)
f7 :: Matroid Int
f7 = fromGeoRep [] [] [[1,2,3],[1,4,7],[1,5,6],[2,4,6],[2,5,7],[3,4,5],[3,6,7]] [[1..7]]

-- Oxley p36, fig 1.12:
-- cycleMatroid [[1,2],[1,3],[2,3],[3,4],[2,4],[1,4]] == restriction fanoPlane [1..6]

-- |F7-, the relaxation of the Fano plane by removal of a line
f7m :: Matroid Int
f7m = relaxation f7 [2,4,6]

-- Oxley p39
-- |The Pappus configuration from projective geometry
pappus :: Matroid Int
pappus = fromGeoRep [] [] [[1,2,3],[1,5,7],[1,6,8],[2,4,7],[2,6,9],[3,4,8],[3,5,9],[4,5,6],[7,8,9]] [[1..9]]

-- more logical relabeling
-- pappus' = fromGeoRep [] [] [[1,2,3],[1,5,9],[1,6,8],[2,4,9],[2,6,7],[3,4,8],[3,5,7],[4,5,6],[7,8,9]] [[1..9]]

-- |Relaxation of the Pappus configuration by removal of a line
nonPappus :: Matroid Int
nonPappus = relaxation pappus [7,8,9]
-- fromGeoRep [] [] [[1,2,3],[1,5,7],[1,6,8],[2,4,7],[2,6,9],[3,4,8],[3,5,9],[4,5,6]] [[1..9]]

-- |The Desargues configuration
desargues :: Matroid Int
desargues = fromGeoRep [] [] [[1,2,5],[1,3,6],[1,4,7],[2,3,8],[2,4,9],[3,4,10],[5,6,8],[5,7,9],[6,7,10],[8,9,10]]
                              [[1,2,3,5,6,8],[1,2,4,5,7,9],[1,3,4,6,7,10],[2,3,4,8,9,10],[5,6,7,8,9,10]]
-- desargues == cycleMatroid (combinationsOf 2 [1..5])
-- (ie Desargues = M(K5) )
-- interestingly, although these are all the dependent flats that are evident from the diagram,
-- there are also some rank 3 flats consisting of a line and an "antipodal" point
-- eg, in terms of K5, [1,8,9,10] corresponds to [[1,2],[3,4],[3,5],[4,5]]

-- Oxley p71
vamosMatroid1 = fromHyperplanes [1..8] (hs4 ++ hs3)
    where hs4 = [ [1,2,3,4], [1,4,5,6], [2,3,5,6], [1,4,7,8], [2,3,7,8] ]
          hs3 = [ h3 | h3 <- combinationsOf 3 [1..8], null [h4 | h4 <- hs4, h3 `LS.isSubset` h4] ]
vamosMatroid = fromGeoRep [] [] [] [[1,2,3,4],[1,4,5,6],[2,3,5,6],[1,4,7,8],[2,3,7,8]]

-- |The Vamos matroid V8. It is not representable over any field.
v8 :: Matroid Int
v8 = vamosMatroid
-- v8 is self-dual (isomorphic to its own dual)

-- Oxley p188
-- |P8 is a minor-minimal matroid that is not representable over F4, F8, F16, ... .
-- It is Fq-representable if and only if q is not a power of 2.
p8 :: Matroid Int
p8 = vectorMatroid $
    ( [ [1,0,0,0,  0, 1, 1,-1],
        [0,1,0,0,  1, 0, 1, 1],
        [0,0,1,0,  1, 1, 0, 1],
        [0,0,0,1, -1, 1, 1, 0] ] :: [[F3]] )

p8' = fromGeoRep [] [] [] [ [1,2,3,8], [1,2,4,7], [1,3,4,6], [1,4,5,8], [1,5,6,7], [2,3,4,5], [2,3,6,7], [2,5,6,8], [3,5,7,8], [4,6,7,8] ]

-- |P8- is a relaxation of P8. It is Fq-representable if and only if q >= 4.
p8m :: Matroid Int
p8m = relaxation p8 [2,3,6,7]

-- |P8-- is a relaxation of P8-. It is a minor-minimal matroid that is not representable over F4.
-- It is Fq-representable if and only if q >= 5.
p8mm :: Matroid Int
p8mm = relaxation p8m [1,4,5,8]


-- Oxley p317
-- r-spoked wheel graph
wheelGraph r = G.G vs es
    where vs = [0..r]
          es = [ [0,i] | i <- [1..r] ] ++ [ [i,i+1] | i <- [1..r-1] ] ++ [ [1,r] ]
-- for normal form, should L.sort es

mw4 = cycleMatroid $ G.edges $ wheelGraph 4
-- has [5,6,7,8] as unique circuit-hyperplane

w4' = relaxation mw4 $ unique $ circuitHyperplanes mw4 -- [5,6,7,8]

-- Oxley p183
w4 = fromGeoRep [] [] [[1,2,5],[1,4,8],[2,3,6],[3,4,7]] [[1,2,3,5,6],[1,2,4,5,8],[1,3,4,7,8],[2,3,4,6,7]]


-- BINARY MATROIDS

-- Oxley p344
isBinary2 m = all (even . length) [ c `LS.intersect` cc | c <- circuits m, cc <- cocircuits m ]


-- RANK POLYNOMIAL
-- Godsil & Royle p356

[x,y] = map glexVar ["x","y"] :: [GlexPoly Integer String]

-- first naive version
rankPoly1 m = sum [ x^(rm - r a) * y^(rm' - r' a') | a <- powersetdfs es, let a' = es LS.\\ a ]
    where es = elements m
          rm = rank m
          r = rankfun m
          m' = dual m
          rm' = rank m'
          r' = rankfun m'

-- |Given a matroid m over elements es, the rank polynomial is a polynomial r(x,y),
-- which is essentially a generating function for the subsets of es, enumerated by size and rank.
-- It is efficiently calculated using deletion and contraction.
--
-- It has the property that r(0,0) is the number of bases in m, r(1,0) is the number of independent sets,
-- r(0,1) is the number of spanning sets. It can also be used to derive the chromatic polynomial of a graph,
-- the weight enumerator of a linear code, and more.
rankPoly :: (Ord a) => Matroid a -> GlexPoly Integer String
rankPoly m
    | null es = 1
    | isLoop m e = (1+y) * rankPoly (m \\\ [e]) -- deletion
    | isColoop m e = (1+x) * rankPoly (m /// [e]) -- contraction
    | otherwise = rankPoly (m \\\ [e]) + rankPoly (m /// [e])
    where es = elements m
          e = head es

numBases m = unwrap $ rankPoly m `bind` (\v -> case v of "x" -> 0; "y" -> 0)

numIndeps m = unwrap $ rankPoly m `bind` (\v -> case v of "x" -> 1; "y" -> 0)

numSpanning m = unwrap $ rankPoly m `bind` (\v -> case v of "x" -> 0; "y" -> 1)

-- It is then possible to derive the chromatic poly of a graph from the rankPoly of its cycle matroid, etc


-- Oxley p586

-- How many independent sets are there of each size
indepCounts m = map length $ L.groupBy eqfst $ [(length i, i) | i <- indeps m]
-- relying on shortlex order

{-
-- The following tries to calculate the indep counts
-- as the Hilbert series of the Stanley-Reisner ring.
-- However, it's not giving the right answer.
indepCounts2 m = take r $ indepCounts' (circuits m)
    where n = length (elements m) -- the number of "variables"
          r = rank m
          n' = toInteger n
          indepCounts' [] = map (\k -> (k+n'-1) `choose` (n'-1)) [0..]
              -- the number of ways of choosing n-1 separators, so as to leave a product of k powers
          indepCounts' (c:cs) =
              let d = length c -- the "degree" of c
                  cs' = reduce [] $ toShortlex $ toSet $ map (LS.\\ c) cs -- the quotient of cs by c
              in indepCounts' cs <-> (replicate d 0 ++ indepCounts' cs')
          reduce ls (r:rs) = if any (`LS.isSubset` r) ls then reduce ls rs else reduce (r:ls) rs
          reduce ls [] = reverse ls
          (x:xs) <+> (y:ys) = (x+y) : (xs <+> ys)
          xs <+> ys = xs ++ ys -- one of them is null
          xs <-> ys = xs <+> map negate ys 
-}


-- Whitney numbers of the second kind
-- The number of flats of each rank
whitney2nd m = map length $ L.groupBy eqfst $ L.sort [(rankfun m f, f) | f <- flats m]

-- Whitney numbers of the first kind
-- The number of subsets (of the element set) of each rank
whitney1st m = alternatingSign $ map length $ L.groupBy eqfst $ L.sort [(rankfun m x, x) | x <- powersetdfs (elements m)]
    where alternatingSign (x:xs) = x : alternatingSign (map negate xs)
          alternatingSign [] = []



-- TODO

-- 1. Sort out the isomorphism code in the case where the incidence graph isn't connected

-- 2. We could generate the geometric representation from a matroid (provided its rank <= 4)
-- geoRep m = filter (isDependent m) (flats m)

-- 3. isMatroidFlats

