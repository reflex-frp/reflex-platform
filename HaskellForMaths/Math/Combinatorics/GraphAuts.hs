-- Copyright (c) David Amos, 2009. All rights reserved.

module Math.Combinatorics.GraphAuts (isVertexTransitive, isEdgeTransitive,
                                     isArcTransitive, is2ArcTransitive, is3ArcTransitive, isnArcTransitive,
                                     isDistanceTransitive,
                                     graphAuts, incidenceAuts,
                                     graphIsos, incidenceIsos,
                                     isGraphIso, isIncidenceIso) where

import Data.Either (lefts)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Math.Common.ListSet
import Math.Core.Utils (combinationsOf, pairs)
import Math.Combinatorics.Graph
-- import Math.Combinatorics.StronglyRegularGraph
-- import Math.Combinatorics.Hypergraph -- can't import this, creates circular dependency
import Math.Algebra.Group.PermutationGroup
import Math.Algebra.Group.SchreierSims as SS


-- The code for finding automorphisms - "graphAuts" - follows later on in file


-- TRANSITIVITY PROPERTIES OF GRAPHS

-- |A graph is vertex-transitive if its automorphism group acts transitively on the vertices. Thus, given any two distinct vertices, there is an automorphism mapping one to the other.
isVertexTransitive :: (Ord t) => Graph t -> Bool
isVertexTransitive (G [] []) = True -- null graph is trivially vertex transitive
isVertexTransitive g@(G (v:vs) es) = orbitV auts v == v:vs where
    auts = graphAuts g

-- |A graph is edge-transitive if its automorphism group acts transitively on the edges. Thus, given any two distinct edges, there is an automorphism mapping one to the other.
isEdgeTransitive :: (Ord t) => Graph t -> Bool
isEdgeTransitive (G _ []) = True
isEdgeTransitive g@(G vs (e:es)) = orbitE auts e == e:es where
    auts = graphAuts g

arc ->^ g = map (.^ g) arc
-- unlike edges/blocks, arcs are directed, so the action on them does not sort

-- Godsil & Royle 59-60
-- |A graph is arc-transitive (or flag-transitive) if its automorphism group acts transitively on arcs. (An arc is an ordered pair of adjacent vertices.)
isArcTransitive :: (Ord t) => Graph t -> Bool
isArcTransitive (G _ []) = True -- empty graphs are trivially arc transitive
isArcTransitive g@(G vs es) = orbit (->^) a auts == a:as where
-- isArcTransitive g@(G vs es) = closure [a] [ ->^ h | h <- auts] == a:as where
    a:as = L.sort $ es ++ map reverse es
    auts = graphAuts g

isArcTransitive' g@(G (v:vs) es) =
    orbitP auts v == v:vs && -- isVertexTransitive g
    orbitP stab n == n:ns
    where auts = graphAuts g
          stab = dropWhile (\p -> v .^ p /= v) auts -- we know that graphAuts are returned in this order
          n:ns = nbrs g v

-- execution time of both of the above is dominated by the time to calculate the graph auts, so their performance is similar


-- then k n, kb n n, q n, other platonic solids, petersen graph, heawood graph, pappus graph, desargues graph are all arc-transitive


-- find arcs of length l from x using dfs - results returned in order
-- an arc is a sequence of vertices connected by edges, no doubling back, but self-crossings allowed
findArcs g@(G vs es) x l = map reverse $ dfs [ ([x],0) ] where
    dfs ( (z1:z2:zs,l') : nodes)
        | l == l'   = (z1:z2:zs) : dfs nodes
        | otherwise = dfs $ [(w:z1:z2:zs,l'+1) | w <- nbrs g z1, w /= z2] ++ nodes
    dfs ( ([z],l') : nodes)
        | l == l'   = [z] : dfs nodes
        | otherwise = dfs $ [([w,z],l'+1) | w <- nbrs g z] ++ nodes
    dfs [] = []

-- note that a graph with triangles can't be 3-arc transitive, etc, because an aut can't map a self-crossing arc to a non-self-crossing arc

-- |A graph is n-arc-transitive is its automorphism group is transitive on n-arcs. (An n-arc is an ordered sequence (v0,...,vn) of adjacent vertices, with crossings allowed but not doubling back.)
isnArcTransitive :: (Ord t) => Int -> Graph t -> Bool
isnArcTransitive _ (G [] []) = True
isnArcTransitive n g@(G (v:vs) es) =
    orbitP auts v == v:vs && -- isVertexTransitive g
    orbit (->^) a stab == a:as
    -- closure [a] [ ->^ h | h <- stab] == a:as
    where auts = graphAuts g
          stab = dropWhile (\p -> v .^ p /= v) auts -- we know that graphAuts are returned in this order
          a:as = findArcs g v n

is2ArcTransitive :: (Ord t) => Graph t -> Bool
is2ArcTransitive g = isnArcTransitive 2 g

is3ArcTransitive :: (Ord t) => Graph t -> Bool
is3ArcTransitive g = isnArcTransitive 3 g

-- Godsil & Royle 66-7
-- |A graph is distance transitive if given any two ordered pairs of vertices (u,u') and (v,v') with d(u,u') == d(v,v'),
-- there is an automorphism of the graph that takes (u,u') to (v,v')
isDistanceTransitive :: (Ord t) => Graph t -> Bool
isDistanceTransitive (G [] []) = True
isDistanceTransitive g@(G (v:vs) es)
    | isConnected g =
        orbitP auts v == v:vs && -- isVertexTransitive g
        length stabOrbits == diameter g + 1 -- the orbits under the stabiliser of v coincide with the distance partition from v
    | otherwise = error "isDistanceTransitive: only defined for connected graphs"
    where auts = graphAuts g
          stab = dropWhile (\p -> v .^ p /= v) auts -- we know that graphAuts are returned in this order
          stabOrbits = let os = orbits stab in os ++ map (:[]) ((v:vs) L.\\ concat os) -- include fixed point orbits


-- GRAPH AUTOMORPHISMS

-- !! Note, in the literature the following is just called the intersection of two partitions
-- !! Refinement actually refers to the process of refining to an equitable partition

-- refine one partition by another
refine p1 p2 = filter (not . null) $ refine' p1 p2
-- Refinement preserves ordering within cells but not between cells
-- eg the cell [1,2,3,4] could be refined to [2,4],[1,3]

-- refine, but leaving null cells in
-- we use this in the graphAuts functions when comparing two refinements to check that they split in the same way
refine' p1 p2 = concat [ [c1 `intersect` c2 | c2 <- p2] | c1 <- p1]


isGraphAut (G vs es) h = all (`S.member` es') [e -^ h | e <- es]
    where es' = S.fromList es
-- this works best on sparse graphs, where p(edge) < 1/2
-- if p(edge) > 1/2, it would be better to test on the complement of the graph




-- Calculate a map consisting of neighbour lists for each vertex in the graph
-- If a vertex has no neighbours then it is left out of the map
adjLists (G vs es) = adjLists' M.empty es
    where adjLists' nbrs ([u,v]:es) =
              adjLists' (M.insertWith' (flip (++)) v [u] $ M.insertWith' (flip (++)) u [v] nbrs) es
          adjLists' nbrs [] = nbrs


-- ALTERNATIVE VERSIONS OF GRAPH AUTS
-- (showing how we got to the final version)

-- return all graph automorphisms, using naive depth first search
graphAuts1 (G vs es) = dfs [] vs vs
    where dfs xys (x:xs) ys =
              concat [dfs ((x,y):xys) xs (L.delete y ys) | y <- ys, isCompatible (x,y) xys]
          dfs xys [] [] = [fromPairs xys]
          isCompatible (x,y) xys = and [([x',x] `S.member` es') == (L.sort [y,y'] `S.member` es') | (x',y') <- xys]
          es' = S.fromList es

-- return generators for graph automorphisms
-- (using Lemma 9.1.1 from Seress p203 to prune the search tree)
graphAuts2 (G vs es) = graphAuts' [] vs
    where graphAuts' us (v:vs) =
              let uus = zip us us
              in concat [take 1 $ dfs ((v,w):uus) vs (v : L.delete w vs) | w <- vs, isCompatible (v,w) uus]
              ++ graphAuts' (v:us) vs
              -- stab us == transversal for stab (v:us) ++ stab (v:us)  (generators thereof)
          graphAuts' _ [] = [] -- we're not interested in finding the identity element
          dfs xys (x:xs) ys =
              concat [dfs ((x,y):xys) xs (L.delete y ys) | y <- ys, isCompatible (x,y) xys]
          dfs xys [] [] = [fromPairs xys]
          isCompatible (x,y) xys = and [([x',x] `S.member` es') == (L.sort [y,y'] `S.member` es') | (x',y') <- xys]
          es' = S.fromList es

-- Now using distance partitions
-- Note that because of the use of distance partitions, this is only valid for connected graphs
graphAuts3 g@(G vs es) = graphAuts' [] [vs] where
    graphAuts' us ((x:ys):pt) =
        let px = refine' (ys : pt) (dps M.! x)
            p y = refine' ((x : L.delete y ys) : pt) (dps M.! y)
            uus = zip us us
            p' = L.sort $ filter (not . null) $ px
        in concat [take 1 $ dfs ((x,y):uus) px (p y) | y <- ys]
        ++ graphAuts' (x:us) p'
    graphAuts' us ([]:pt) = graphAuts' us pt
    graphAuts' _ [] = []
    dfs xys p1 p2
        | map length p1 /= map length p2 = []
        | otherwise =
            let p1' = filter (not . null) p1
                p2' = filter (not . null) p2
            in if all isSingleton p1'
               then let xys' = xys ++ zip (concat p1') (concat p2')
                    in if isCompatible xys' then [fromPairs' xys'] else []
                    -- we shortcut the search when we have all singletons, so must check isCompatible to avoid false positives
               else let (x:xs):p1'' = p1'
                        ys:p2'' = p2'
                    in concat [dfs ((x,y):xys)
                                   (refine' (xs : p1'') (dps M.! x))
                                   (refine' ((L.delete y ys):p2'') (dps M.! y))
                                   | y <- ys]
    isCompatible xys = and [([x,x'] `S.member` es') == (L.sort [y,y'] `S.member` es') | (x,y) <- xys, (x',y') <- xys, x < x']
    dps = M.fromList [(v, distancePartition g v) | v <- vs]
    es' = S.fromList es

isSingleton [_] = True
isSingleton _ = False


-- Now we try to use generators we've already found at a given level to save us having to look for others
-- For example, if we have found (1 2)(3 4) and (1 3 2), then we don't need to look for something taking 1 -> 4
graphAuts4 g@(G vs es) = graphAuts' [] [vs] where
    graphAuts' us p@((x:ys):pt) =
        -- let p' = L.sort $ filter (not . null) $ refine' (ys:pt) (dps M.! x)
        let p' = L.sort $ refine (ys:pt) (dps M.! x)
        in level us p x ys []
        ++ graphAuts' (x:us) p'
    graphAuts' us ([]:pt) = graphAuts' us pt
    graphAuts' _ [] = []
    level us p@(ph:pt) x (y:ys) hs =
        let px = refine' (L.delete x ph : pt) (dps M.! x)
            py = refine' (L.delete y ph : pt) (dps M.! y)
            uus = zip us us
        in case dfs ((x,y):uus) px py of
           []  -> level us p x ys hs
           h:_ -> let hs' = h:hs in h : level us p x (ys L.\\ (x .^^ hs')) hs'
    level _ _ _ [] _ = []
    dfs xys p1 p2
        | map length p1 /= map length p2 = []
        | otherwise =
            let p1' = filter (not . null) p1
                p2' = filter (not . null) p2
            in if all isSingleton p1'
               then let xys' = xys ++ zip (concat p1') (concat p2')
                    in if isCompatible xys' then [fromPairs' xys'] else []
               else let (x:xs):p1'' = p1'
                        ys:p2'' = p2'
                    in concat [dfs ((x,y):xys)
                                   (refine' (xs : p1'') (dps M.! x))
                                   (refine' ((L.delete y ys):p2'') (dps M.! y))
                                   | y <- ys]
    isCompatible xys = and [([x,x'] `S.member` es') == (L.sort [y,y'] `S.member` es') | (x,y) <- xys, (x',y') <- xys, x < x']
    dps = M.fromList [(v, distancePartition g v) | v <- vs]
    es' = S.fromList es

-- contrary to first thought, you can't stop when a level is null - eg kb 2 3, the third level is null, but the fourth isn't



-- an example for equitable partitions
-- this is a graph whose distance partition (from any vertex) can be refined to an equitable partition
eqgraph = G vs es where
    vs = [1..14]
    es = L.sort $ [[1,14],[2,13]] ++ [ [v1,v2] | [v1,v2] <- combinationsOf 2 vs, v1+1 == v2 || v1+3 == v2 && even v2]

-- refine a partition to give an equitable partition
toEquitable g cells = L.sort $ toEquitable' [] cells where
    toEquitable' ls (r:rs) =
        let (lls,lrs) = L.partition isSingleton $ map (splitNumNbrs r) ls
            -- so the lrs split, and the lls didn't
            rs' = concatMap (splitNumNbrs r) rs
        in if isSingleton r -- then we know it won't split further, so can remove it from further processing
           then r : toEquitable' (concat lls) (concat lrs ++ rs')
           else toEquitable' (r : concat lls) (concat lrs ++ rs')
    toEquitable' ls [] = ls
    splitNumNbrs t c = map (map snd) $ L.groupBy (\x y -> fst x == fst y) $ L.sort
                    [ (length ((nbrs_g M.! v) `intersect` t), v) | v <- c]
    nbrs_g = M.fromList [(v, nbrs g v) | v <- vertices g]


-- try to refine two partitions in parallel, failing if they become mismatched
toEquitable2 nbrs_g psrc ptrg = unzip $ L.sort $ toEquitable' [] (zip psrc ptrg) where
    toEquitable' ls (r:rs) =
        let ls' = map (splitNumNbrs nbrs_g r) ls
            (lls,lrs) = L.partition isSingleton $ map fromJust ls'
            rs' = map (splitNumNbrs nbrs_g r) rs
        in if any isNothing ls' || any isNothing rs'
           then []
           else
               {- if (isSingleton . fst) r
               then r : toEquitable' (concat lls) (concat lrs ++ concatMap fromJust rs')
               else -} toEquitable' (r : concat lls) (concat lrs ++ concatMap fromJust rs')
    toEquitable' ls [] = ls

splitNumNbrs nbrs_g (t_src,t_trg) (c_src,c_trg) =
    let src_split = L.groupBy (\x y -> fst x == fst y) $ L.sort
                    [ (length ((nbrs_g M.! v) `intersect` t_src), v) | v <- c_src]
        trg_split = L.groupBy (\x y -> fst x == fst y) $ L.sort
                    [ (length ((nbrs_g M.! v) `intersect` t_trg), v) | v <- c_trg]
    in if map length src_split == map length trg_split
       && map (fst . head) src_split == map (fst . head) trg_split
       then Just $ zip (map (map snd) src_split) (map (map snd) trg_split)
       else Nothing
       -- else error (show (src_split, trg_split)) -- for debugging

-- Now, every time we intersect two partitions, refine to an equitable partition

-- |Given a graph g, @graphAuts g@ returns generators for the automorphism group of g.
-- If g is connected, then the generators will be a strong generating set.
graphAuts :: (Ord a) => Graph a -> [Permutation a]
graphAuts g = autsWithinComponents ++ isosBetweenComponents
    where cs = map (inducedSubgraph g) (components g)
          -- autsWithinComponents = concatMap graphAutsCon cs
          autsWithinComponents = concatMap graphAuts4 cs
          isosBetweenComponents = map swapFromIso $ concat [take 1 (graphIsos ci cj) | (ci,cj) <- pairs cs]
          swapFromIso xys = fromPairs (xys ++ map swap xys)
          swap (x,y) = (y,x)
-- Using graphAuts4 instead of graphAutsCon as latter appears to have a bug, eg
-- > graphAuts4 $ G [1..3] [[1,2],[2,3]]
-- [[[1,3]]]
-- > graphAutsCon $ G [1..3] [[1,2],[2,3]]
-- []

-- Automorphisms of a connected graph
graphAutsCon g@(G vs es)
    | isConnected g = graphAuts' [] (toEquitable g $ valencyPartition g)
    | otherwise = error "graphAutsCon: graph is not connected"
    where graphAuts' us p@((x:ys):pt) =
              let p' = L.sort $ filter (not . null) $ refine' (ys:pt) (dps M.! x)
              in level us p x ys []
              ++ graphAuts' (x:us) p'
          graphAuts' us ([]:pt) = graphAuts' us pt
          graphAuts' _ [] = []
          level us p@(ph:pt) x (y:ys) hs =
              let px = refine' (L.delete x ph : pt) (dps M.! x)
                  py = refine' (L.delete y ph : pt) (dps M.! y)
                  uus = zip us us
              in case dfsEquitable (dps,es',nbrs_g) ((x,y):uus) px py of
                 []  -> level us p x ys hs
                 h:_ -> let hs' = h:hs in h : level us p x (ys L.\\ (x .^^ hs')) hs'
          level _ _ _ [] _ = []
          dps = M.fromList [(v, distancePartition g v) | v <- vs]
          es' = S.fromList es
          nbrs_g = M.fromList [(v, nbrs g v) | v <- vs]

dfsEquitable (dps,es',nbrs_g) xys p1 p2 = dfs xys p1 p2 where
    dfs xys p1 p2
        | map length p1 /= map length p2 = []
        | otherwise =
            let p1' = filter (not . null) p1
                p2' = filter (not . null) p2
                (p1e,p2e) = toEquitable2 nbrs_g p1' p2'
            in if null p1e
               then []
               else
                   if all isSingleton p1e
                   then let xys' = xys ++ zip (concat p1e) (concat p2e)
                        in if isCompatible xys' then [fromPairs' xys'] else []
                   else let (x:xs):p1'' = p1e
                            ys:p2'' = p2e
                        in concat [dfs ((x,y):xys)
                                       (refine' (xs : p1'') (dps M.! x))
                                       (refine' ((L.delete y ys):p2'') (dps M.! y))
                                       | y <- ys]
    isCompatible xys = and [([x,x'] `S.member` es') == (L.sort [y,y'] `S.member` es') | (x,y) <- xys, (x',y') <- xys, x < x']


-- AUTS OF INCIDENCE STRUCTURE VIA INCIDENCE GRAPH

-- based on graphAuts as applied to the incidence graph, but modified to avoid point-block crossover auts

-- |Given the incidence graph of an incidence structure between points and blocks
-- (for example, a set system),
-- @incidenceAuts g@ returns generators for the automorphism group of the incidence structure.
-- The generators are represented as permutations of the points.
-- The incidence graph should be represented with the points on the left and the blocks on the right.
-- If the incidence graph is connected, then the generators will be a strong generating set.
incidenceAuts :: (Ord p, Ord b) => Graph (Either p b) -> [Permutation p]
incidenceAuts g = autsWithinComponents ++ isosBetweenComponents
    where cs = map (inducedSubgraph g) (components g)
          autsWithinComponents = concatMap incidenceAutsCon cs
          isosBetweenComponents = map swapFromIso $ concat [take 1 (incidenceIsos ci cj) | (ci,cj) <- pairs cs]
          swapFromIso xys = fromPairs (xys ++ map swap xys)
          swap (x,y) = (y,x)

incidenceAutsCon g@(G vs es) 
    | isConnected g = map points (incidenceAuts' [] [vs])
    | otherwise = error "incidenceAutsCon: graph is not connected"
    where points h = fromPairs [(x,y) | (Left x, Left y) <- toPairs h] -- filtering out the action on blocks
          incidenceAuts' us p@((x@(Left _):ys):pt) =
              -- let p' = L.sort $ filter (not . null) $ refine' (ys:pt) (dps M.! x)
              let p' = L.sort $ refine (ys:pt) (dps M.! x)
              in level us p x ys []
              ++ incidenceAuts' (x:us) p'
          incidenceAuts' us ([]:pt) = incidenceAuts' us pt
          incidenceAuts' _ (((Right _):_):_) = [] -- if we fix all the points, then the blocks must be fixed too
          incidenceAuts' _ [] = []
          level us p@(ph:pt) x (y@(Left _):ys) hs =
              let px = refine' (L.delete x ph : pt) (dps M.! x)
                  py = refine' (L.delete y ph : pt) (dps M.! y)
                  uus = zip us us
              in case dfsEquitable (dps,es',nbrs_g) ((x,y):uus) px py of
                 []  -> level us p x ys hs
                 h:_ -> let hs' = h:hs in h : level us p x (ys L.\\ (x .^^ hs')) hs'
          level _ _ _ _ _ = [] -- includes the case where y matches Right _, which can only occur on first level, before we've distance partitioned
          dps = M.fromList [(v, distancePartition g v) | v <- vs]
          es' = S.fromList es
          nbrs_g = M.fromList [(v, nbrs g v) | v <- vs]


-- GRAPH ISOMORPHISMS

-- !! not yet using equitable partitions, so could probably be more efficient


-- graphIsos :: (Ord a, Ord b) => Graph a -> Graph b -> [[(a,b)]]
graphIsos g1 g2
    | length cs1 /= length cs2 = []
    | otherwise = graphIsos' cs1 cs2
    where cs1 = map (inducedSubgraph g1) (components g1)
          cs2 = map (inducedSubgraph g2) (components g2)
          graphIsos' (ci:cis) cjs =
              [iso ++ iso' | cj <- cjs,
                             iso <- graphIsosCon ci cj,
                             let cjs' = L.delete cj cjs,
                             iso' <- graphIsos' cis cjs']
          graphIsos' [] [] = [[]]

-- isos between connected graphs
graphIsosCon g1 g2 
    | isConnected g1 && isConnected g2
        = concat [dfs [] (distancePartition g1 v1) (distancePartition g2 v2)
                 | v1 <- take 1 (vertices g1), v2 <- vertices g2]
                 -- the take 1 handles the case where g1 is the null graph
    | otherwise = error "graphIsosCon: either or both graphs are not connected"
    where dfs xys p1 p2
              | map length p1 /= map length p2 = []
              | otherwise =
                  let p1' = filter (not . null) p1
                      p2' = filter (not . null) p2
                  in if all isSingleton p1'
                     then let xys' = xys ++ zip (concat p1') (concat p2')
                          in if isCompatible xys' then [xys'] else []
                     else let (x:xs):p1'' = p1'
                              ys:p2'' = p2'
                          in concat [dfs ((x,y):xys)
                                         (refine' (xs : p1'') (dps1 M.! x))
                                         (refine' ((L.delete y ys):p2'') (dps2 M.! y))
                                         | y <- ys]
          isCompatible xys = and [([x,x'] `S.member` es1) == (L.sort [y,y'] `S.member` es2) | (x,y) <- xys, (x',y') <- xys, x < x']
          dps1 = M.fromList [(v, distancePartition g1 v) | v <- vertices g1]
          dps2 = M.fromList [(v, distancePartition g2 v) | v <- vertices g2]
          es1 = S.fromList $ edges g1
          es2 = S.fromList $ edges g2


-- |Are the two graphs isomorphic?
isGraphIso :: (Ord a, Ord b) => Graph a -> Graph b -> Bool
isGraphIso g1 g2 = (not . null) (graphIsos g1 g2)
-- !! If we're only interested in seeing whether or not two graphs are iso,
-- !! then the cost of calculating distancePartitions may not be warranted
-- !! (see Math.Combinatorics.Poset: orderIsos01 versus orderIsos)

-- !! deprecate
isIso g1 g2 = (not . null) (graphIsos g1 g2)


-- the following differs from graphIsos in only two ways
-- we avoid Left, Right crossover isos, by insisting that a Left is taken to a Left (first two lines)
-- we return only the action on the Lefts, and unLeft it
-- incidenceIsos :: (Ord p1, Ord b1, Ord p2, Ord b2) =>
--     Graph (Either p1 b1) -> Graph (Either p2 b2) -> [[(p1,p2)]]

incidenceIsos g1 g2
    | length cs1 /= length cs2 = []
    | otherwise = incidenceIsos' cs1 cs2
    where cs1 = map (inducedSubgraph g1) (filter (not . null . lefts) $ components g1)
          cs2 = map (inducedSubgraph g2) (filter (not . null . lefts) $ components g2)
          incidenceIsos' (ci:cis) cjs =
              [iso ++ iso' | cj <- cjs,
                             iso <- incidenceIsosCon ci cj,
                             let cjs' = L.delete cj cjs,
                             iso' <- incidenceIsos' cis cjs']
          incidenceIsos' [] [] = [[]]

incidenceIsosCon g1 g2
    | isConnected g1 && isConnected g2
        = concat [dfs [] (distancePartition g1 v1) (distancePartition g2 v2)
                 | v1@(Left _) <- take 1 (vertices g1), v2@(Left _) <- vertices g2]
                 -- g1 may have no vertices
    | otherwise = error "incidenceIsos: one or both graphs not connected"
    where dfs xys p1 p2
              | map length p1 /= map length p2 = []
              | otherwise =
                  let p1' = filter (not . null) p1
                      p2' = filter (not . null) p2
                  in if all isSingleton p1'
                     then let xys' = xys ++ zip (concat p1') (concat p2')
                          in if isCompatible xys' then [[(x,y) | (Left x, Left y) <- xys']] else []
                     else let (x:xs):p1'' = p1'
                              ys:p2'' = p2'
                          in concat [dfs ((x,y):xys)
                                         (refine' (xs : p1'') (dps1 M.! x))
                                         (refine' ((L.delete y ys):p2'') (dps2 M.! y))
                                         | y <- ys]
          isCompatible xys = and [([x,x'] `S.member` es1) == (L.sort [y,y'] `S.member` es2) | (x,y) <- xys, (x',y') <- xys, x < x']
          dps1 = M.fromList [(v, distancePartition g1 v) | v <- vertices g1]
          dps2 = M.fromList [(v, distancePartition g2 v) | v <- vertices g2]
          es1 = S.fromList $ edges g1
          es2 = S.fromList $ edges g2

-- |Are the two incidence structures represented by these incidence graphs isomorphic?
isIncidenceIso :: (Ord p1, Ord b1, Ord p2, Ord b2) =>
     Graph (Either p1 b1) -> Graph (Either p2 b2) -> Bool
isIncidenceIso g1 g2 = (not . null) (incidenceIsos g1 g2)

{-
removeGens x gs = removeGens' [] gs where
    baseOrbit = x .^^ gs
    removeGens' ls (r:rs) =
        if x .^^ (ls++rs) == baseOrbit
        then removeGens' ls rs
        else removeGens' (r:ls) rs
    removeGens' ls [] = reverse ls
-- !! reverse is probably pointless


-- !! DON'T THINK THIS IS WORKING PROPERLY
-- eg graphAutsSGSNew $ toGraph ([1..7],[[1,3],[2,3],[3,4],[4,5],[4,6],[4,7]])
-- returns [[[1,2]],[[5,6]],[[5,7,6]],[[6,7]]]
-- whereas [[6,7]] was a Schreier generator, so shouldn't have been listed

-- Using Schreier generators to seed the next level
-- At the moment this is slower than the above
-- (This could be modified to allow us to start the search with a known subgroup)
graphAutsNew g@(G vs es) = graphAuts' [] [] [vs] where
    graphAuts' us hs p@((x:ys):pt) =
        let ys' = ys L.\\ (x .^^ hs) -- don't need to consider points which can already be reached from Schreier generators
            hs' = level us p x ys' []
            p' = L.sort $ filter (not . null) $ refine' (ys:pt) (dps M.! x)
            reps = cosetRepsGx (hs'++hs) x
            schreierGens = removeGens x $ schreierGeneratorsGx (x,reps) (hs'++hs)
        in hs' ++ graphAuts' (x:us) schreierGens p'
    graphAuts' us hs ([]:pt) = graphAuts' us hs pt
    graphAuts' _ _ [] = []
    level us p@(ph:pt) x (y:ys) hs =
        let px = refine' (L.delete x ph : pt) (dps M.! x)
            py = refine' (L.delete y ph : pt) (dps M.! y)
            uus = zip us us
        in if map length px /= map length py
           then level us p x ys hs
           else case dfs ((x,y):uus) (filter (not . null) px) (filter (not . null) py) of
                []  -> level us p x ys hs
                h:_ -> let hs' = h:hs in h : level us p x (ys L.\\ (x .^^ hs')) hs'
                -- if h1 = (1 2)(3 4), and h2 = (1 3 2), then we can remove 4 too
    level _ _ _ [] _ = []
    dfs xys p1 p2
        | map length p1 /= map length p2 = []
        | otherwise =
            let p1' = filter (not . null) p1
                p2' = filter (not . null) p2
            in if all isSingleton p1'
               then let xys' = xys ++ zip (concat p1') (concat p2')
                    in if isCompatible xys' then [fromPairs' xys'] else []
               else let (x:xs):p1'' = p1'
                        ys:p2'' = p2'
                    in concat [dfs ((x,y):xys)
                                   (refine' (xs : p1'') (dps M.! x))
                                   (refine' ((L.delete y ys):p2'') (dps M.! y))
                                   | y <- ys]
    isCompatible xys = and [([x,x'] `S.member` es') == (L.sort [y,y'] `S.member` es') | (x,y) <- xys, (x',y') <- xys, x < x']
    dps = M.fromList [(v, distancePartition g v) | v <- vs]
    es' = S.fromList es
-}


