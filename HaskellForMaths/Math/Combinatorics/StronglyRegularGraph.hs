-- Copyright (c) 2008, David Amos. All rights reserved.

-- |A module defining various strongly regular graphs, including the Clebsch, Hoffman-Singleton, Higman-Sims, and McLaughlin graphs.
--
-- A strongly regular graph with parameters (n,k,lambda,mu) is a (simple) graph with n vertices,
-- in which the number of common neighbours of x and y is k, lambda or mu according as whether
-- x and y are equal, adjacent, or non-adjacent. (In particular, it is a k-regular graph.)
--
-- Strongly regular graphs are highly symmetric, and have large automorphism groups.
module Math.Combinatorics.StronglyRegularGraph where

import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as S

import Math.Common.ListSet
import Math.Core.Utils (combinationsOf)
import Math.Algebra.Group.PermutationGroup hiding (P)
import Math.Algebra.Group.SchreierSims as SS
import Math.Combinatorics.Graph as G hiding (G)
import Math.Combinatorics.GraphAuts
import Math.Combinatorics.Design as D
import Math.Algebra.LinearAlgebra -- hiding (t)
import Math.Algebra.Field.Base -- for F2
import Math.Combinatorics.FiniteGeometry

-- Sources
-- Godsil & Royle, Algebraic Graph Theory
-- Cameron & van Lint, Designs, Graphs, Codes and their Links
-- van Lint & Wilson, A Course in Combinatorics, 2nd ed


-- STRONGLY REGULAR GRAPHS

-- strongly regular graphs
srgParams g
    | null es = error "srgParams: not defined for null graph"
    | null es' = error "srgParams: not defined for complete graph"
    | otherwise =
        if all (==k) ks && all (==lambda) ls && all (==mu) ms
        then Just (n,k,lambda,mu)
        else Nothing
    where vs = vertices g
          n = length vs
          es = edges g
          es' = combinationsOf 2 vs \\ es -- the non-edges
          k:ks = map (valency g) vs
          lambda:ls = map (length . commonNbrs) es  -- common neighbours of adjacent vertices
          mu:ms = map (length . commonNbrs) es' -- common neighbours of non-adjacent vertices
          commonNbrs [v1,v2] = (nbrs_g M.! v1) `intersect` (nbrs_g M.! v2)
          nbrs_g = M.fromList [ (v, nbrs g v) | v <- vs ]

isSRG g = isJust $ srgParams g


-- SIMPLE EXAMPLES

-- Triangular graph - van Lint & Wilson p262
-- http://mathworld.wolfram.com/TriangularGraph.html
t' m = G.to1n $ t m

t m | m >= 4 = graph (vs,es) where
    vs = combinationsOf 2 [1..m]
    es = [ [v,v'] | v <- vs, v' <- dropWhile (<= v) vs, not (disjoint v v')]
-- This is just lineGraph (k m), by another name


-- Lattice graph - van Lint & Wilson p262
-- http://mathworld.wolfram.com/LatticeGraph.html
l2' m = G.to1n $ l2 m

l2 m | m >= 2 = graph (vs,es) where
    vs = [ (i,j) | i <- [1..m], j <- [1..m] ]
    es = [ [v,v'] | v@(i,j) <- vs, v'@(i',j') <- dropWhile (<= v) vs, i == i' || j == j']
-- This is lineGraph (kb m m)
-- Automorphism group is Sm * Sm * C2
-- via i -> ig, j -> jg, i <-> j

paleyGraph fq | length fq `mod` 4 == 1 = graph (vs,es) where
    vs = fq
    qs = set [x^2 | x <- vs] \\ [0] -- the non-zero squares in Fq
    es = [ [x,y] | x <- vs, y <- vs, x < y, (x-y) `elem` qs]


-- CLEBSCH GRAPH
-- van Lint & Wilson, p263

clebsch' = G.to1n clebsch

clebsch = graph (vs,es) where
    vs = L.sort $ filter (even . length) $ powerset [1..5]
    es = [ [v,v'] | v <- vs, v' <- dropWhile (<= v) vs, length (symDiff v v') == 4]

-- Alternative construction from Cameron & van Lint p106
clebsch2 = graph (vs,es) where
    D xs bs = pairDesign 5
    vs = [C] ++ [P x | x <- xs] ++ [B b | b <- bs]
    es = L.sort $ [ [B a, B b]  | a <- bs, b <- dropWhile (<=a) bs, disjoint a b]
               ++ [ [P p, B b] | b <- bs, p <- b]
               ++ [ [C, P p]   | p <- xs ]


-- HOFFMAN-SINGLETON GRAPH
-- Cameron, Permutation Groups, p79ff
-- Godsil & Royle, p92ff
-- Aut group is U3(5).2 (Atlas p34)

triples = combinationsOf 3 [1..7]

heptads = [ [a,b,c,d,e,f,g] | a <- triples,
                              b <- triples, a < b, meetOne b a,
                              c <- triples, b < c, all (meetOne c) [a,b],
                              d <- triples, c < d, all (meetOne d) [a,b,c],
                              e <- triples, d < e, all (meetOne e) [a,b,c,d],
                              f <- triples, e < f, all (meetOne f) [a,b,c,d,e],
                              g <- triples, f < g, all (meetOne g) [a,b,c,d,e,f],
                              foldl intersect [1..7] [a,b,c,d,e,f,g] == [] ]
    where meetOne x y = length (intersect x y) == 1
    -- each pair of triples meet in exactly one point, and there is no point in all of them - Godsil & Royle p69
    -- (so these are the projective planes over 7 points)

plane +^ g = L.sort [line -^ g | line <- plane]

plane +^^ gs = orbit (+^) plane gs
-- plane +^^ gs = closure [plane] [ +^ g | g <- gs ]

hoffmanSingleton' = G.to1n hoffmanSingleton

hoffmanSingleton = graph (vs,es) where
    h = head heptads
    hs = h +^^ _A 7 -- an A7 orbit of a heptad
    vs = map Left hs ++ map Right triples
    es = [ [Left h, Right t] | h <- hs, t <- triples, t `elem` h]
      ++ [ [Right t, Right t'] | t <- triples, t' <- dropWhile (<= t) triples, t `disjoint` t']

-- induced action of A7 on Hoffman-Singleton graph
inducedA7 g = fromPairs [(v, v ~^ g) | v <- vs] where
    vs = vertices hoffmanSingleton
    (Left h) ~^ g = Left (h +^ g)
    (Right t) ~^ g = Right (t -^ g)

hsA7 = toSn $ map inducedA7 $ _A 7


-- GEWIRTZ GRAPH
-- van Lint & Wilson p266-7
-- (also called Sims-Gewirtz graph)

gewirtz' = G.to1n gewirtz

gewirtz = graph (vs,es) where
    vs = [xs | xs <- blocks s_3_6_22, 22 `notElem` xs]
    -- The 21 blocks of S(3,6,22) which contain 22 are the lines of PG(2,4) (projective plane over F4)
    -- The 56 blocks which don't are hyperovals in this plane. They form a 2-(21,6,4) design.
    es = [ [v,v'] | v <- vs, v' <- dropWhile (<= v) vs, length (v `intersect` v') == 0]


-- HIGMAN-SIMS GRAPH
-- Aut group is HS.2, where HS is the Higman-Sims sporadic simple group

data DesignVertex = C | P Integer | B [Integer] deriving (Eq,Ord,Show)

higmanSimsGraph' = G.to1n higmanSimsGraph

-- Cameron & van Lint, p107
higmanSimsGraph = graph (vs,es) where
    D xs bs = s_3_6_22
    vs = [C] ++ [P x | x <- xs] ++ [B b | b <- bs]
    es = L.sort $ [ [B a, B b]  | a <- bs, b <- dropWhile (<=a) bs, disjoint a b]
               ++ [ [P p, B b] | b <- bs, p <- b]
               ++ [ [C, P p]   | p <- xs ]
    -- s_3_6_22' = blocks s_3_6_22

-- There is an induced action of M22 on Higman Sims graph

-- induced action of M22 on Higman-Sims graph
inducedM22 g = fromPairs [(v, v ~^ g) | v <- vs] where
    -- G vs _ = higmanSimsGraph'
    vs = vertices higmanSimsGraph
    (B b) ~^ g = B (b -^ g)
    (P p) ~^ g = P (p .^ g)
    C ~^ _ = C

higmanSimsM22 = toSn $ map inducedM22 $ m22sgs
-- all (isGraphAut higmanSimsGraph) higmanSimsM22

-- M22 is one point stabilizer (of C)

-- HS.2, where HS is Higman-Sims sporadic group
_HS2 = SS.reduceGens $ graphAuts higmanSimsGraph
-- (It will actually find 11 strong generators, but the first 4 are sufficient to generate the group)

_HS = SS.derivedSubgp _HS2



-- SYMPLECTIC GRAPHS

-- Godsil & Royle p242
sp2 r = graph (vs,es) where
    vs = tail $ ptsAG (2*r) f2 -- all non-zero pts in F2^2r
    es = [ [u,v] | [u,v] <- combinationsOf 2 vs, u <*>> n <.> v == 1] -- uT N v == 1, ie vectors adjacent if non-orthogonal
    n = fMatrix (2*r) (\i j -> if abs (i-j) == 1 && even (max i j) then 1 else 0) -- matrix defining a symplectic form

sp n | even n = sp2 (n `div` 2)


-- TWO GRAPHS AND SWITCHING

-- SCHLAFLI GRAPH
-- An srg(27,16,10,8)
-- Has geometric interpretation in terms of 27 lines on general cubic surface in projective 3-space
-- Aut group is G.2 where G = U4(2) = S4(3) (Atlas p26)
-- (G.2 is also the Weyl group of E6 - don't know if there's any connection)

-- Godsil & Royle p254ff
switch g us | us `D.isSubset` vs = graph (vs, L.sort switchedes) where
    vs = vertices g
    us' = vs L.\\ us -- complement of us in vs
    es = edges g
    es' = S.fromList es
    switchedes = [e | e@[v1,v2] <- es, (v1 `elem` us) == (v2 `elem` us)]
                 -- edges within us or its complement are unchanged
              ++ [ L.sort [v1,v2] | v1 <- us, v2 <- us', L.sort [v1,v2] `S.notMember` es']
                 -- edges between us and its complement are switched

-- Godsil & Royle p259
schlafli' = G.to1n schlafli

schlafli = graph (vs,es') where
    g = lineGraph $ k 8
    v:vs = vertices g
    es = edges g
    gswitched = switch g (nbrs g v) -- switch off the vertex v
    es' = edges gswitched


-- MCLAUGHLIN GRAPH
-- Aut group is McL.2, where McL is the McLaughlin sporadic simple group
-- http://people.csse.uwa.edu.au/gordon/constructions/mclaughlin/
-- http://mathworld.wolfram.com/McLaughlinGraph.html

mcLaughlin' = G.to1n mcLaughlin

mcLaughlin = graph (vs',es') where
    D xs bs = s_4_7_23
    vs = map P xs ++ map B bs
    es = [ [P x, B b] | x <- xs, b <- bs, x `notElem` b]
      ++ [ [B b1, B b2] | b1 <- bs, b2 <- bs, b1 < b2, length (b1 `intersect` b2) == 1]
    g276 = graph (vs,es)
    g276switched = switch g276 (nbrs g276 (P 0))
    P 0 : vs' = vs -- drop P 0 as it's now not connected
    es' = edges g276switched

_McL2 = SS.reduceGens $ graphAuts mcLaughlin
-- finds 14 auts - but takes half an hour (interpreted) to do so
-- in fact just the first 2 are sufficient to generate the group

_McL = SS.derivedSubgp $ _McL2

{-
-- TWO GRAPH ON 276 VERTICES
-- Has Conway's .3 as automorphism group

-- Godsil & Royle p260ff

twoGraph276 =
    let nt = D.incidenceMatrix s_4_7_23
        n = L.transpose nt -- Godsil & Royle do incidence matrix the other way round to us
        s = L.transpose $
            (j 23 23 <<->> i 23)      +|+ (j 23 253 <<->> 2 *>> n)
                                      ++
            (j 253 23 <<->> 2 *>> nt) +|+ (nt <<*>> n <<->> 5 *>> i 253 <<->> 2 *>> j 253 253)
        a = (map . map) (`div` 2) (j 276 276 <<->> i 276 <<->> s)
    in fromAdjacencyMatrix a
    where j r c = replicate r (replicate c 1)
          i = idMx
          (+|+) = zipWith (++)

-- Its automorphism group *as a two-graph* is .3 (Co3)
-- But its aut group as a graph is only M23

twoGraph276' = graph (vs,es) where
    D xs bs = s_4_7_23
    vs = map P xs ++ map B bs
    es = [ [P x, B b] | x <- xs, b <- bs, x `notElem` b]
      ++ [ [B b1, B b2] | b1 <- bs, b2 <- bs, b1 < b2, length (b1 `intersect` b2) == 1]
-- !! This isn't isomorphic to twoGraph276
-- (Perhaps it is in the same switching class though)
-- We can obtain McLaughlin graph from this by switching in neighbourhood of P 0
-}