-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}

-- |A module for working with directed graphs (digraphs).
-- Some of the functions are specifically for working with directed acyclic graphs (DAGs),
-- that is, directed graphs containing no cycles.
module Math.Combinatorics.Digraph where

import Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

toSet = S.toList . S.fromList

-- |A digraph is represented as DG vs es, where vs is the list of vertices, and es is the list of edges.
-- Edges are directed: an edge (u,v) means an edge from u to v.
-- A digraph is considered to be in normal form if both es and vs are in ascending order.
-- This is the preferred form, and some functions will only work for digraphs in normal form.
data Digraph v = DG [v] [(v,v)] deriving (Eq,Ord,Show)

instance Functor Digraph where
    -- |If f is not order-preserving, then you should call nf afterwards
    fmap f (DG vs es) = DG (map f vs) (map (\(u,v)->(f u, f v)) es)

nf (DG vs es) = DG (L.sort vs) (L.sort es)

vertices (DG vs _) = vs

edges (DG _ es) = es


-- Is it valid to call them predecessors / successors in the case when the digraph contains cycles?

predecessors (DG _ es) v = [u | (u,v') <- es, v' == v]

successors (DG _ es) u = [v | (u',v) <- es, u' == u]

-- Calculate maps of predecessor and successor lists for each vertex in a digraph.
-- If a vertex has no predecessors (respectively successors), then it is left out of the relevant map
adjLists (DG vs es) = adjLists' (M.empty, M.empty) es
    where adjLists' (preds,succs) ((u,v):es) =
              adjLists' (M.insertWith' (flip (++)) v [u] preds, M.insertWith' (flip (++)) u [v] succs) es
          adjLists' (preds,succs) [] = (preds, succs)


digraphIsos1 (DG vsa esa) (DG vsb esb)
    | length vsa /= length vsb = []
    | length esa /= length esb = []
    | otherwise = digraphIsos' [] vsa vsb
    where digraphIsos' xys [] [] = [xys]
          digraphIsos' xys (x:xs) ys =
              concat [ digraphIsos' ((x,y):xys) xs (L.delete y ys)
                     | y <- ys, isCompatible (x,y) xys]
          isCompatible (x,y) xys = and [ ((x,x') `elem` esa) == ((y,y') `elem` esb)
                                      && ((x',x) `elem` esa) == ((y',y) `elem` esb)
                                       | (x',y') <- xys ]

digraphIsos2 a b
    | length (vertices a) /= length (vertices b) = []
    | L.sort (M.elems indega) /= L.sort (M.elems indegb) = [] 
    | L.sort (M.elems outdega) /= L.sort (M.elems outdegb) = [] 
    | otherwise = dfs [] (vertices a) (vertices b)
    where (preda,succa) = adjLists a
          (predb,succb) = adjLists b
          indega = M.map length preda
          indegb = M.map length predb
          outdega = M.map length succa
          outdegb = M.map length succb
          isCompatible (x,y) xys = (M.findWithDefault 0 x indega) == (M.findWithDefault 0 y indegb)
                                && (M.findWithDefault 0 x outdega) == (M.findWithDefault 0 y outdegb)
                                && and [ (x' `elem` predx) == (y' `elem` predy)
                                      && (x' `elem` succx) == (y' `elem` succy)
                                       | let predx = M.findWithDefault [] x preda, let predy = M.findWithDefault [] y predb,
                                         let succx = M.findWithDefault [] x succa, let succy = M.findWithDefault [] y succb,
                                         (x',y') <- xys]
          dfs xys [] [] = [xys]
          dfs xys (x:xs) ys =
              concat [ dfs ((x,y):xys) xs (L.delete y ys)
                     | y <- ys, isCompatible (x,y) xys]

-- For DAGs, can almost certainly do better than the above by using the height partition
-- However see remarks in Poset on orderIsos:
-- What is most efficient will depend on whether you want to list all of them, or just find out whether there are any or not
-- Could also try refining the height partition by (indegree,outdegree)


-- doesn't check whether input is a dag
-- if not, then the output will not contain all the vs
heightPartitionDAG dag@(DG vs es) = heightPartition' S.empty [v | v <- vs, v `M.notMember` preds] -- ie vertices with no predecessors
    where (preds,succs) = adjLists dag
          heightPartition' interior boundary
              | null boundary = []
              | otherwise = let interior' = S.union interior $ S.fromList boundary
                                boundary' = toSet [v | u <- boundary, v <- M.findWithDefault [] u succs,
                                                       all (`S.member` interior') (preds M.! v) ]
                            in boundary : heightPartition' interior' boundary'

isDAG dag@(DG vs _) = length vs == length (concat (heightPartitionDAG dag))

-- Only valid for DAGs, not for digraphs in general
dagIsos dagA@(DG vsA esA) dagB@(DG vsB esB)
    | length vsA /= length (concat heightPartA) = error "dagIsos: dagA is not a DAG"
    | length vsB /= length (concat heightPartB) = error "dagIsos: dagB is not a DAG"
    | map length heightPartA /= map length heightPartB = []
    | otherwise = dfs [] heightPartA heightPartB
    where heightPartA = heightPartitionDAG dagA
          heightPartB = heightPartitionDAG dagB
          (predsA,_) = adjLists dagA
          (predsB,_) = adjLists dagB
          dfs xys [] [] = [xys]
          dfs xys ([]:las) ([]:lbs) = dfs xys las lbs
          dfs xys ((x:xs):las) (ys:lbs) =
              concat [ dfs ((x,y):xys) (xs:las) (L.delete y ys : lbs)
                     | y <- ys, isCompatible (x,y) xys]
          isCompatible (x,y) xys =
              let preds_x = M.findWithDefault [] x predsA
                  preds_y = M.findWithDefault [] y predsB
              in and [ (x' `elem` preds_x) == (y' `elem` preds_y) | (x',y') <- xys]
              -- and [ ((x',x) `elem` esA) == ((y',y) `elem` esB)
              --     | (x',y') <- xys ]
          -- we only need to check predecessors, not successors, because we proceeding by height ordering

-- can probably do better by intersecting the height partition with the (indegree,outdegree) partition
-- (although on very symmetrical posets such as B n, this won't help at all)

-- |Are the two DAGs isomorphic?
isDagIso :: (Ord a, Ord b) => Digraph a -> Digraph b -> Bool
isDagIso dagA dagB = (not . null) (dagIsos dagA dagB)


perms [] = [[]]
perms (x:xs) = [ls ++ [x] ++ rs | ps <- perms xs, (ls,rs) <- zip (inits ps) (tails ps)]
-- or use L.permutations

{-
-- orderings compatible with the height partition
heightOrderingsDAG dag@(DG vs es) = heightOrderings' [[]] (heightPartitionDAG dag)
    where heightOrderings' initsegs (level:levels) =
              let addsegs = perms level
                  initsegs' = [init ++ add | init <- initsegs, add <- addsegs]
              in heightOrderings' initsegs' levels
          heightOrderings' segs [] = segs
-}

isoRepDAG1 dag@(DG vs es) = isoRepDAG' [M.empty] 1 (heightPartitionDAG dag)
    where isoRepDAG' initmaps j (level:levels) =
              let j' = j + length level
                  addmaps = [M.fromList (zip ps [j..]) | ps <- perms level]
                  initmaps' = [init +++ add | init <- initmaps, add <- addmaps]
              in isoRepDAG' initmaps' j' levels
          isoRepDAG' maps _ [] = DG [1..length vs] (minimum [L.sort (map (\(u,v) -> (m M.! u, m M.! v)) es) | m <- maps])
          initmap +++ addmap = M.union initmap addmap

-- For example
-- > isoRepDAG1 (DG ['a'..'e'] [('a','c'),('a','d'),('b','d'),('b','e'),('d','e')])
-- ([1,2,3,4,5],[(1,3),(1,4),(2,3),(2,5),(3,5)])
-- > isoRepDAG1 (DG ['a'..'e'] [('a','d'),('a','e'),('b','c'),('b','d'),('d','e')])
-- ([1,2,3,4,5],[(1,3),(1,4),(2,3),(2,5),(3,5)])


-- Find the minimum height-preserving numberings of the vertices, using dfs
isoRepDAG2 dag@(DG vs es) = minimum $ dfs [] srclevels trglevels
    where -- (preds,succs) = adjLists dag
          srclevels = heightPartitionDAG dag
          trglevels = reverse $ fst $ foldl
                      (\(tls,is) sl -> let (js,ks) = splitAt (length sl) is in (js:tls,ks))
                      ([],[1..]) srclevels
          dfs xys [] [] = [xys]
          dfs xys ([]:sls) ([]:tls) = dfs xys sls tls
          dfs xys ((x:xs):sls) (ys:tls) =
              concat [ dfs ((x,y):xys) (xs:sls) (L.delete y ys : tls) | y <- ys]
              -- not applying any compatibility condition yet


-- Find the height-respecting numbering of the vertices which leads to the minimal numbering of the edges
-- So this is calculating the same function as isoRepDAG1, but more efficiently
-- Uses dfs with pruning, rather than exhaustive search
isoRepDAG3 dag@(DG vs es) = dfs root [root]
    where n = length vs
          root = ([],(1,0),M.empty,(srclevels,trglevels)) -- root of the search tree
          (preds,succs) = adjLists dag
          srclevels = heightPartitionDAG dag
          trglevels = reverse $ fst $ foldl
                      (\(tls,is) sl -> let (js,ks) = splitAt (length sl) is in (js:tls,ks))
                      ([],[1..]) srclevels
          dfs best (node:stack) =
              -- node : -- for debugging
              case cmpPartial best node of
              LT -> dfs best stack                      -- ie prune the search tree at this node
              GT -> dfs node (successors node ++ stack) -- ie replace best with this node
              EQ -> dfs best (successors node ++ stack)
          -- dfs best [] = [best] -- !! for debugging
          dfs best@(es',_,_,_) [] = DG [1..n] es'
          successors (es,_,_,([],[])) = []
          successors (es,(i,j),m,([]:sls,[]:tls)) = successors (es,(i,j),m,(sls,tls))
          successors (es,(i,j),m,(xs:sls,(y:ys):tls)) =
              [ (es', (i',y), m', (L.delete x xs : sls, ys : tls))
              | x <- xs,
                let m' = M.insert x y m,
                let es' = L.sort $ es ++ [(m M.! u, y) | u <- M.findWithDefault [] x preds],
                let i' = nextunfinished m' i ]
          -- a vertex is considered finished when all its successors have assignments in the map
          nextunfinished m i =
              case [v | (v,i') <- M.assocs m, i' == i] of
              [] -> i
              [u] -> if all (`M.member` m) (M.findWithDefault [] u succs)
                     then nextunfinished m (i+1) -- i is finished: all successors already have assignments in the map
                     else i
          cmpPartial (es,_,_,_) (es',(i',j'),_,_) = 
              cmpPartial' (i',j') es es'
              -- where j' = maximum $ 0 : map snd es'
          cmpPartial' (i',j') ((u,v):es) ((u',v'):es') =
          -- Any new e' that can be added to es' must be greater than (i',j')
          -- (we don't care about possible extensions of es, because we're not extending them)
              case compare (u,v) (u',v') of
              EQ -> cmpPartial' (i',j') es es'
              LT -> if (u,v) <= (i',j') then LT else EQ
              GT -> GT -- always replace best if you beat it
                       -- (even if it could improve, it's not going to as we're not progressing it)
          cmpPartial' (i',j') ((u,v):es) [] = if (u,v) <= (i',j') then LT else EQ
          cmpPartial' _ [] ((u',v'):es') = GT -- always extend an existing partial best
          cmpPartial' _ [] [] = EQ


-- Now we seek a numbering of the vertices which respects height-ordering,
-- and within each height level respects (indegree,outdegree) ordering.
-- We seek the numbering which minimises the resulting edge list.


-- |Given a directed acyclic graph (DAG), return a canonical representative for its isomorphism class.
-- @isoRepDAG dag@ is isomorphic to @dag@. It follows that if @isoRepDAG dagA == isoRepDAG dagB@ then @dagA@ is isomorphic to @dagB@.
-- Conversely, @isoRepDAG dag@ is the minimal element in the isomorphism class, subject to some constraints.
-- It follows that if @dagA@ is isomorphic to @dagB@, then @isoRepDAG dagA == isoRepDAG dagB@.
--
-- The algorithm of course is faster on some DAGs than others: roughly speaking,
-- it prefers \"tall\" DAGs (long chains) to \"wide\" DAGs (long antichains),
-- and it prefers asymmetric DAGs (ie those with smaller automorphism groups).
isoRepDAG :: (Ord a) => Digraph a -> Digraph Int
isoRepDAG dag@(DG vs es) = dfs root [root]
    where n = length vs
          root = ([],(1,0),M.empty,(srclevels,trglevels)) -- root of the search tree
          (preds,succs) = adjLists dag
          indegs = M.map length preds
          outdegs = M.map length succs
          byDegree vs = (map . map) snd $ L.groupBy (\(du,u) (dv,v) -> du == dv) $ L.sort
                        [( (M.findWithDefault 0 v indegs, M.findWithDefault 0 v outdegs), v) | v <- vs]
          srclevels = concatMap byDegree $ heightPartitionDAG dag
          trglevels = reverse $ fst $ foldl
                      (\(tls,is) sl -> let (js,ks) = splitAt (length sl) is in (js:tls,ks))
                      ([],[1..]) srclevels
          dfs best (node:stack) =
              -- node : -- for debugging
              case cmpPartial best node of
              LT -> dfs best stack                      -- ie prune the search tree at this node
              GT -> dfs node (successors node ++ stack) -- ie replace best with this node
              EQ -> dfs best (successors node ++ stack)
          -- dfs best [] = [best] -- !! for debugging
          dfs best@(es',_,_,_) [] = DG [1..n] es'
          successors (es,_,_,([],[])) = []
          successors (es,(i,j),m,([]:sls,[]:tls)) = successors (es,(i,j),m,(sls,tls))
          successors (es,(i,j),m,(xs:sls,(y:ys):tls)) =
              [ (es', (i',y), m', (L.delete x xs : sls, ys : tls))
              | x <- xs,
                let m' = M.insert x y m,
                let es' = L.sort $ es ++ [(m M.! u, y) | u <- M.findWithDefault [] x preds],
                let i' = nextunfinished m' i ]
          -- a vertex is considered finished when all its successors have assignments in the map
          nextunfinished m i =
              case [v | (v,i') <- M.assocs m, i' == i] of
              [] -> i
              [u] -> if all (`M.member` m) (M.findWithDefault [] u succs)
                     then nextunfinished m (i+1) -- i is finished: all successors already have assignments in the map
                     else i
          cmpPartial (es,_,_,_) (es',(i',j'),_,_) = 
              cmpPartial' (i',j') es es'
              -- where j' = maximum $ 0 : map snd es'
          cmpPartial' (i',j') ((u,v):es) ((u',v'):es') =
          -- Any new e' that can be added to es' must be greater than (i',j')
          -- (we don't care about possible extensions of es, because we're not extending them)
              case compare (u,v) (u',v') of
              EQ -> cmpPartial' (i',j') es es'
              LT -> if (u,v) <= (i',j') then LT else EQ
              GT -> GT -- always replace best if you beat it
                       -- (even if it could improve, it's not going to as we're not progressing it)
          cmpPartial' (i',j') ((u,v):es) [] = if (u,v) <= (i',j') then LT else EQ
          cmpPartial' _ [] ((u',v'):es') = GT -- always extend an existing partial best
          cmpPartial' _ [] [] = EQ
