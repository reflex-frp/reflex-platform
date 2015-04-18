-- Copyright (c) 2008-2011, David Amos. All rights reserved.

-- |A module defining a polymorphic data type for (simple, undirected) graphs,
-- together with constructions of some common families of graphs,
-- new from old constructions, and calculation of simple properties of graphs.
module Math.Combinatorics.Graph where

import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow ( (&&&) )

import Math.Common.ListSet as LS
import Math.Core.Utils
import Math.Algebra.Group.PermutationGroup hiding (fromDigits, fromBinary)
import Math.Algebra.Group.SchreierSims as SS

-- Main source: Godsil & Royle, Algebraic Graph Theory


-- COMBINATORICS
-- Some functions we'll use

set xs = map head $ L.group $ L.sort xs

-- subsets of a set (returned in "binary" order)
powerset [] = [[]]
powerset (x:xs) = let p = powerset xs in p ++ map (x:) p

-- GRAPH

-- |Datatype for graphs, represented as a list of vertices and a list of edges.
-- For most purposes, graphs are required to be in normal form.
-- A graph G vs es is in normal form if (i) vs is in ascending order without duplicates,
-- (ii) es is in ascending order without duplicates, (iii) each e in es is a 2-element list [x,y], x<y
data Graph a = G [a] [[a]] deriving (Eq,Ord,Show)

instance Functor Graph where
    -- |If f is not order-preserving, then you should call nf afterwards
    fmap f (G vs es) = G (map f vs) (map (map f) es)
-- could use DeriveFunctor to derive this Functor instance

-- |Convert a graph to normal form. The input is assumed to be a valid graph apart from order
nf :: Ord a => Graph a -> Graph a
nf (G vs es) = G vs' es' where
    vs' = L.sort vs
    es' = L.sort (map L.sort es)

-- we require that vs, es, and each individual e are sorted
isSetSystem xs bs = isListSet xs && isListSet bs && all isListSet bs && all (`isSubset` xs) bs

isGraph vs es = isSetSystem vs es && all ( (==2) . length) es

-- |Safe constructor for graph from lists of vertices and edges.
-- graph (vs,es) checks that vs and es are valid before returning the graph.
graph :: (Ord t) => ([t], [[t]]) -> Graph t
graph (vs,es) | isGraph vs es = G vs es
--              | otherwise = error ( "graph " ++ show (vs,es) )
-- isValid g = g where g = G vs es

toGraph (vs,es) | isGraph vs' es' = G vs' es' where
    vs' = L.sort vs
    es' = L.sort $ map L.sort es
-- note that calling isListSet on a sorted list still checks that there are no duplicates

vertices (G vs _) = vs

edges (G _ es) = es


-- OTHER REPRESENTATIONS

-- incidence matrix of a graph
-- (rows and columns indexed by edges and vertices respectively)
-- (warning: in the literature it is often the other way round)
incidenceMatrix (G vs es) = [ [if v `elem` e then 1 else 0 | v <- vs] | e <- es]

fromIncidenceMatrix m = graph (vs,es) where
    n = L.genericLength $ head m
    vs = [1..n]
    es = L.sort $ map edge m
    edge row = [v | (1,v) <- zip row vs]

adjacencyMatrix (G vs es) =
    [ [if L.sort [i,j] `S.member` es' then 1 else 0 | j <- vs] | i <- vs]
    where es' = S.fromList es

fromAdjacencyMatrix m = graph (vs,es) where
    n = L.genericLength m
    vs = [1..n]
    es = es' 1 m
    es' i (r:rs) = [ [i,j] | (j,1) <- drop i (zip vs r)] ++ es' (i+1) rs
    es' _ [] = []

-- SOME SIMPLE FAMILIES OF GRAPHS

-- |The null graph on n vertices is the graph with no edges
nullGraph :: (Integral t) => t -> Graph t
nullGraph n = G [1..n] []

-- |The null graph, with no vertices or edges
nullGraph' :: Graph Int -- type signature needed
nullGraph' = G [] []

-- |c n is the cyclic graph on n vertices
c :: (Integral t) => t -> Graph t
c n = graph (vs,es) where
    vs = [1..n]
    es = L.insert [1,n] [[i,i+1] | i <- [1..n-1]]
-- automorphism group is D2n

-- |k n is the complete graph on n vertices
k :: (Integral t) => t -> Graph t
k n = graph (vs,es) where
    vs = [1..n]
    es = [[i,j] | i <- [1..n-1], j <- [i+1..n]] -- == combinationsOf 2 [1..n]
-- automorphism group is Sn

-- |kb m n is the complete bipartite graph on m and n vertices
kb :: (Integral t) => t -> t -> Graph t
kb m n = to1n $ kb' m n

-- |kb' m n is the complete bipartite graph on m left and n right vertices
kb' :: (Integral t) => t -> t -> Graph (Either t t)
kb' m n = graph (vs,es) where
    vs = map Left [1..m] ++ map Right [1..n]
    es = [ [Left i, Right j] | i <- [1..m], j <- [1..n] ]
-- automorphism group is Sm*Sn (plus a flip if m==n)

-- |q k is the graph of the k-cube
q :: (Integral t) => Int -> Graph t
q k = fromBinary $ q' k

q' :: (Integral t) => Int -> Graph [t]
q' k = graph (vs,es) where
    vs = sequence $ replicate k [0,1] -- ptsAn k f2
    es = [ [u,v] | [u,v] <- combinationsOf 2 vs, hammingDistance u v == 1 ]
    hammingDistance as bs = length $ filter id $ zipWith (/=) as bs
-- can probably type-coerce this to be Graph [F2] if required


tetrahedron = k 4

cube = q 3

octahedron = graph (vs,es) where
    vs = [1..6]
    es = combinationsOf 2 vs L.\\ [[1,6],[2,5],[3,4]]

dodecahedron = toGraph (vs,es) where
    vs = [1..20]
    es = [ [1,2],[2,3],[3,4],[4,5],[5,1],
           [6,7],[7,8],[8,9],[9,10],[10,11],[11,12],[12,13],[13,14],[14,15],[15,6],
           [16,17],[17,18],[18,19],[19,20],[20,16],
           [1,6],[2,8],[3,10],[4,12],[5,14],
           [7,16],[9,17],[11,18],[13,19],[15,20] ]

icosahedron = toGraph (vs,es) where
    vs = [1..12]
    es = [ [1,2],[1,3],[1,4],[1,5],[1,6],
           [2,3],[3,4],[4,5],[5,6],[6,2],
           [7,12],[8,12],[9,12],[10,12],[11,12],
           [7,8],[8,9],[9,10],[10,11],[11,7],
           [2,7],[7,3],[3,8],[8,4],[4,9],[9,5],[5,10],[10,6],[6,11],[11,2] ]



-- convert a graph to have [1..n] as vertices
to1n (G vs es) = graph (vs',es') where
    mapping = M.fromList $ zip vs [1..] -- the mapping from vs to [1..n]
    vs' = M.elems mapping
    es' = [map (mapping M.!) e | e <- es] -- the edges will already be sorted correctly by construction

-- |Given a graph with vertices which are lists of small integers, eg [1,2,3],
-- return a graph with vertices which are the numbers obtained by interpreting these as digits, eg 123.
-- The caller is responsible for ensuring that this makes sense (eg that the small integers are all < 10)
fromDigits :: Integral a => Graph [a] -> Graph a
fromDigits = fmap fromDigits'
{-
fromDigits (G vs es) = graph (vs',es') where
    vs' = map fromDigits' vs
    es' = (map . map) fromDigits' es
-}

-- |Given a graph with vertices which are lists of 0s and 1s,
-- return a graph with vertices which are the numbers obtained by interpreting these as binary digits.
-- For example, [1,1,0] -> 6.
fromBinary :: Integral a => Graph [a] -> Graph a
fromBinary = fmap fromBinary'
{-
fromBinary (G vs es) = graph (vs',es') where
    vs' = map fromBinary' vs
    es' = (map . map) fromBinary' es
-}

petersen :: Graph [Integer]
petersen = graph (vs,es) where
    vs = combinationsOf 2 [1..5]
    es = [ [v1,v2] | [v1,v2] <- combinationsOf 2 vs, disjoint v1 v2]
-- == kneser 5 2 == j 5 2 0
-- == complement $ lineGraph' $ k 5
-- == complement $ t' 5


-- NEW GRAPHS FROM OLD

complement :: (Ord t) => Graph t -> Graph t
complement (G vs es) = graph (vs,es') where es' = combinationsOf 2 vs LS.\\ es
-- es' = [e | e <- combinationsOf 2 vs, e `notElem` es]

-- |The restriction of a graph to a subset of the vertices
restriction :: (Eq a) => Graph a -> [a] -> Graph a
restriction g@(G vs es) us = G us (es `restrict` us)
    where es `restrict` us = [e | e@[i,j] <- es, i `elem` us, j `elem` us]

inducedSubgraph :: (Eq a) => Graph a -> [a] -> Graph a
inducedSubgraph g@(G vs es) us = G us (es `restrict` us)
    where es `restrict` us = [e | e@[i,j] <- es, i `elem` us, j `elem` us]

lineGraph g = to1n $ lineGraph' g

lineGraph' (G vs es) = graph (es, [ [ei,ej] | ei <- es, ej <- dropWhile (<= ei) es, ei `intersect` ej /= [] ])


-- SIMPLE PROPERTIES OF GRAPHS

order g = length (vertices g)

size g = length (edges g)

-- also called degree
valency (G vs es) v = length $ filter (v `elem`) es

valencies g@(G vs es) = map (head &&& length) $ L.group $ L.sort $ map (valency g) vs

valencyPartition g@(G vs es) = map (map snd) $ L.groupBy (\x y -> fst x == fst y) [(valency g v, v) | v <- vs]

regularParam g =
    case valencies g of
    [(v,_)] -> Just v
    _       -> Nothing

-- |A graph is regular if all vertices have the same valency (degree)
isRegular :: (Eq t) => Graph t -> Bool
isRegular g = isJust $ regularParam g

-- |A 3-regular graph is called a cubic graph
isCubic :: (Eq t) => Graph t -> Bool
isCubic g = regularParam g == Just 3


nbrs (G vs es) v = [u | [u,v'] <- es, v == v']
                ++ [w | [v',w] <- es, v == v']
-- if the graph is valid, then the neighbours will be returned in ascending order


-- find paths from x to y using bfs
-- by definition, a path is a subgraph isomorphic to a "line" - it can't have self-crossings
-- (a walk allows self-crossings, a trail allows self-crossings but no edge reuse)
findPaths g@(G vs es) x y = map reverse $ bfs [ [x] ] where
    bfs ((z:zs) : nodes)
        | z == y    = (z:zs) : bfs nodes
        | otherwise = bfs (nodes ++ [(w:z:zs) | w <- nbrs g z, w `notElem` zs])
    bfs [] = []

-- |Within a graph G, the distance d(u,v) between vertices u, v is length of the shortest path from u to v
distance :: (Eq a) => Graph a -> a -> a -> Int
distance g x y =
    case findPaths g x y of
    [] -> -1 -- infinite
    p:ps -> length p - 1

-- |The diameter of a graph is maximum distance between two distinct vertices
diameter :: (Ord t) => Graph t -> Int
diameter g@(G vs es)
    | isConnected g = maximum $ map maxDistance vs
    | otherwise = -1
    where maxDistance v = length (distancePartition g v) - 1

-- find cycles starting at x
-- by definition, a cycle is a subgraph isomorphic to a cyclic graph - it can't have self-crossings
-- (a circuit allows self-crossings but not edge reuse)
findCycles g@(G vs es) x = [reverse (x:z:zs) | z:zs <- bfs [ [x] ], z `elem` nbrsx, length zs > 1] where
    nbrsx = nbrs g x
    bfs ((z:zs) : nodes) = (z:zs) : bfs (nodes ++ [ w:z:zs | w <- nbrs g z, w `notElem` zs])
    bfs [] = []

-- |The girth of a graph is the size of the smallest cycle that it contains.
-- Note: If the graph contains no cycles, we return -1, representing infinity.
girth :: (Eq t) => Graph t -> Int
girth g@(G vs es) = minimum' $ map minCycle vs where
    minimum' xs = let (zs,nzs) = L.partition (==0) xs in if null nzs then -1 else minimum nzs
    minCycle v = case findCycles g v of
                 [] -> 0
                 c:cs -> length c - 1 -- because v occurs twice in c, as startpoint and endpoint

-- circumference = max cycle - Bollobas p104


distancePartition g v = distancePartition' S.empty (S.singleton v) where
    distancePartition' interior boundary
        | S.null boundary = []
        | otherwise = let interior' = S.union interior boundary
                          boundary' = foldl S.union S.empty [S.fromList (nbrs g x) | x <- S.toList boundary] S.\\ interior'
                      in S.toList boundary : distancePartition' interior' boundary'

-- the connected component to which v belongs
component g v = L.sort $ concat $ distancePartition g v

-- |Is the graph connected?
isConnected :: (Ord t) => Graph t -> Bool
isConnected g@(G (v:vs) es) = length (component g v) == length (v:vs)
isConnected (G [] []) = True

components g = components' (vertices g)
    where components' [] = []
          components' (v:vs) = let c = component g v in c : components' (vs LS.\\ c)

-- MORE GRAPHS


-- Generalized Johnson graph, Godsil & Royle p9
-- Also called generalised Kneser graph, http://en.wikipedia.org/wiki/Kneser_graph
j v k i | v >= k && k >= i
    = graph (vs,es) where
        vs = combinationsOf k [1..v]
        es = [ [v1,v2] | [v1,v2] <- combinationsOf 2 vs, length (v1 `intersect` v2) == i ]
-- j v k i is isomorphic to j v (v-k) (v-2k+i), so may as well have v >= 2k

-- kneser v k | v >= 2*k = j v k 0
-- |kneser n k returns the kneser graph KG n,k -
-- whose vertices are the k-element subsets of [1..n], with edges between disjoint subsets
kneser :: Int -> Int -> Graph [Int]
kneser n k | 2*k <= n = graph (vs,es) where
    vs = combinationsOf k [1..n]
    es = [ [v1,v2] | [v1,v2] <- combinationsOf 2 vs, disjoint v1 v2]

johnson v k | v >= 2*k = j v k (k-1)


bipartiteKneser n k | 2*k < n = graph (vs,es) where
    vs = map Left (combinationsOf k [1..n])
      ++ map Right (combinationsOf (n-k) [1..n])
    es = [ [Left u, Right v] | u <- combinationsOf k [1..n], v <- combinationsOf (n-k) [1..n], u `isSubset` v]

desargues1 = bipartiteKneser 5 2


-- Generalised Petersen graphs
-- http://en.wikipedia.org/wiki/Petersen_graph
gp n k | 2*k < n = toGraph (vs,es) where
    vs = map Left [0..n-1] ++ map Right [0..n-1]
    es = (map . map) Left [ [i, (i+1) `mod` n] | i <- [0..n-1] ]
      ++ [ [Left i, Right i] | i <- [0..n-1] ]
      ++ (map . map) Right [ [i, (i+k) `mod` n] | i <- [0..n-1] ]

petersen2 = gp 5 2
prism n = gp n 1
durer = gp 6 2
mobiusKantor = gp 8 3
dodecahedron2 = gp 10 2
desargues2 = gp 10 3