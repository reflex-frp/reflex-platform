-- Copyright (c) 2008, David Amos. All rights reserved.

-- |A module for constructing and working with combinatorial designs.
--
-- Given integers t \< k \< v and lambda > 0, a t-design or t-(v,k,lambda) design is an incidence structure of points X and blocks B,
-- where X is a set of v points, B is a collection of k-subsets of X, with the property that any t points are contained
-- in exactly lambda blocks. If lambda = 1 and t >= 2, then a t-design is also called a Steiner system S(t,k,v).
--
-- Many designs are highly symmetric structures, having large automorphism groups. In particular, the Mathieu groups,
-- which were the first discovered sporadic finite simple groups, turn up as the automorphism groups of the Witt designs.
module Math.Combinatorics.Design where

import Data.Maybe (fromJust, isJust)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Math.Common.ListSet (intersect, symDiff)
import Math.Core.Utils (combinationsOf)

import Math.Algebra.Field.Base
import Math.Algebra.Field.Extension
import Math.Algebra.Group.PermutationGroup hiding (elts, order, isMember)
import Math.Algebra.Group.SchreierSims as SS
import Math.Combinatorics.Graph as G hiding (to1n, incidenceMatrix)
import Math.Combinatorics.GraphAuts (graphAuts, incidenceAuts) -- , removeGens)
import Math.Combinatorics.FiniteGeometry

-- Cameron & van Lint, Designs, Graphs, Codes and their Links


{-
set xs = map head $ group $ sort xs
-}
isSubset xs ys = all (`elem` ys) xs


-- DESIGNS

data Design a = D [a] [[a]] deriving (Eq,Ord,Show)
-- Do we or should we insist on ordering of the xs or bs?

design (xs,bs) | isValid d = d where d = D xs bs

toDesign (xs,bs) = D xs' bs' where
    xs' = L.sort xs
    bs' = L.sort $ map L.sort bs -- in fact don't require that the blocks are in order

isValid (D xs bs) = (xs == L.sort xs || error "design: points are not in order")
                 && (all (\b -> b == L.sort b) bs || error "design: blocks do not have points in order")
-- could also check that each block is a subset of xs, etc

points (D xs bs) = xs

blocks (D xs bs) = bs


-- FINDING DESIGN PARAMETERS

noRepeatedBlocks (D xs bs) = all ( (==1) . length ) $ L.group $ L.sort bs


-- Note that the design parameters functions don't check no repeated blocks, so they're also valid for t-structures

-- given t and a t-(v,k,lambda) design, return (v,k,lambda)
tDesignParams t d =
    case findvk d of
    Nothing -> Nothing
    Just (v,k) ->
        case findlambda t d of
        Nothing -> Nothing
        Just lambda -> Just (v,k,lambda)

findvk (D xs bs) =
    let k:ls = map length bs
    in if all (==k) ls then Just (v,k) else Nothing
    where v = length xs

findlambda t (D xs bs) =
    let lambda:ls = [length [b | b <- bs, ts `isSubset` b] | ts <- combinationsOf t xs]
    in if all (==lambda) ls then Just lambda else Nothing

-- given (xs,bs), return design parameters t-(v,k,lambda) with t maximal
designParams d =
    case findvk d of
    Nothing -> Nothing
    Just (v,k) ->
        case reverse (takeWhile (isJust . snd) [(t, findlambda t d) | t <- [0..k] ]) of
        [] -> Nothing
        (t,Just lambda):_ -> Just (t,(v,k,lambda))
-- Note that a 0-(v,k,lambda) design just means that there are lambda blocks, all of size k, with no other regularity

isStructure t d = isJust $ tDesignParams t d

isDesign t d = noRepeatedBlocks d && isStructure t d

is2Design d = isDesign 2 d

-- square 2-design (more often called "symmetric" in the literature)
isSquare d@(D xs bs) = is2Design d && length xs == length bs


-- (We follow Cameron & van Lint.)
-- |The incidence matrix of a design, with rows indexed by blocks and columns by points.
-- (Note that in the literature, the opposite convention is sometimes used instead.)
incidenceMatrix :: (Eq t) => Design t -> [[Int]]
incidenceMatrix (D xs bs) = [ [if x `elem` b then 1 else 0 | x <- xs] | b <- bs]


-- SOME FAMILIES OF DESIGNS

-- the following is trivially a k-(v,k,lambda) design
subsetDesign v k = design (xs,bs) where
    xs = [1..v]
    bs = combinationsOf k xs

-- Cameron & van Lint, p30
-- the pair design on n points is the complete graph on n points considered as a 2-(n,2,1) design
pairDesign n = D vs es where
    graph = G.k n
    vs = vertices graph
    es = edges graph

-- |The affine plane AG(2,Fq), a 2-(q^2,q,1) design or Steiner system S(2,q,q^2).
ag2 :: (FiniteField k, Ord k) => [k] -> Design [k]
ag2 fq = design (points, lines) where
    points = ptsAG 2 fq
    lines = map line $ tail $ ptsPG 2 fq
    line [a,b,c] = [ [x,y] | [x,y] <- points, a*x+b*y+c==0 ]

-- |The projective plane PG(2,Fq), a square 2-(q^2+q+1,q+1,1) design or Steiner system S(2,q+1,q^2+q+1).
-- For example, @pg2 f2@ is the Fano plane, a Steiner triple system S(2,3,7).
pg2 :: (FiniteField k, Ord k) => [k] -> Design [k]
pg2 fq = design (points, lines) where
    points = ptsPG 2 fq
    lines = L.sort $ map line points
    line u = [v | v <- points, u <.> v == 0]
    u <.> v = sum (zipWith (*) u v)
-- Remember that the points and lines of PG(2,Fp) are really the lines and planes of AG(3,Fp).
-- A line in AG(3,Fp) defines a plane orthogonal to it.


-- The points and i-flats of PG(n,fq), 1<=i<=n-1, form a 2-design
-- For i==1, this is a 2-((q^(n+1)-1)/(q-1),q+1,1) design
-- For i==n-1, this is a 2-((q^(n+1)-1)/(q-1),(q^n-1)/(q-1),(q^(n-1)-1)/(q-1)) design
-- Cameron & van Lint, p8
flatsDesignPG n fq k = design (points, blocks) where
    points = ptsPG n fq
    blocks = map closurePG $ flatsPG n fq k -- the closurePG replaces the generators of the flat by the list of points of the flat

-- The projective point-hyperplane design is also denoted PG(n,q)
pg n fq = flatsDesignPG n fq (n-1)

-- (Cameron & van Lint don't actually state that this is a design except when k == n-1)
flatsDesignAG n fq k = design (points, blocks) where
    points = ptsAG n fq
    blocks = map closureAG $ flatsAG n fq k -- the closureAG replaces the generators of the flat by the list of points of the flat

-- The affine point-hyperplane design is also denoted AG(n,q)
-- It a 2-(q^n,q^(n-1),(q^(n-1)-1)/(q-1)) design
-- Cameron & van Lint, p17
ag n fq = flatsDesignAG n fq (n-1)



-- convert a design to be defined over the set [1..n]
to1n (D xs bs) = (D xs' bs') where
    mapping = M.fromList $ zip xs [1..] -- the mapping from vs to [1..n]
    xs' = M.elems mapping
    bs' = [map (mapping M.!) b | b <- bs] -- the blocks will already be sorted correctly by construction


-- Cameron & van Lint p10
paleyDesign fq | length fq `mod` 4 == 3 = design (xs,bs) where
    xs = fq
    qs = set [x^2 | x <- xs] L.\\ [0] -- the non-zero squares in Fq
    bs = [L.sort (map (x+) qs) | x <- xs]

fanoPlane = paleyDesign f7
-- isomorphic to PG(2,F2)


-- NEW DESIGNS FROM OLD

-- Dual of a design. Cameron & van Lint p11
-- |The dual of a design
dual :: (Ord t) => Design t -> Design [t]
dual (D xs bs) = design (bs, map beta xs) where
    beta x = filter (x `elem`) bs

-- Derived design relative to a point. Cameron & van Lint p11
-- Derived design of a t-(v,k,lambda) is a t-1-(v-1,k-1,lambda) design.
derivedDesign :: (Ord t) => Design t -> t -> Design t
derivedDesign (D xs bs) p = design (xs L.\\ [p], [b L.\\ [p] | b <- bs, p `elem` b])

-- Residual design relative to a point. Cameron & van Lint p13
-- Point-residual of a t-(v,k,lambda) is a t-1-(v-1,k,mu).
pointResidual :: (Ord t) => Design t -> t -> Design t
pointResidual (D xs bs) p = design (xs L.\\ [p], [b | b <- bs, p `notElem` b])

-- Complementary design. Cameron & van Lint p13
-- Complement of a t-(v,k,lambda) is a t-(v,v-k,mu).
complementaryDesign (D xs bs) = design (xs, [xs L.\\ b | b <- bs])

-- Residual design relative to a block. Cameron & van Lint p13
-- This is only a design if (xs,bs) is a square design
-- It may have repeated blocks - but if so, residuals of the complement will not
-- Block-residual of a 2-(v,k,lambda) is a 2-(v-k,k-lambda,lambda).
blockResidual :: (Ord t) => Design t -> [t] -> Design t
blockResidual d@(D xs bs) b | isSquare d = design (xs L.\\ b, [b' L.\\ b | b' <- bs, b' /= b])


-- DESIGN AUTOMORPHISMS

isDesignAut (D xs bs) g | supp g `isSubset` xs = all (`S.member` bs') [b -^ g | b <- bs]
    where bs' = S.fromList bs

-- |The incidence graph of a design
incidenceGraph :: (Ord a) => Design a -> Graph (Either a [a])
incidenceGraph (D xs bs) = G vs es where -- graph (vs,es) where
    vs = L.sort $ map Left xs ++ map Right bs
    es = L.sort [ [Left x, Right b] | x <- xs, b <- bs, x `elem` b ]


-- |Find a strong generating set for the automorphism group of a design
designAuts :: (Ord t) => Design t -> [Permutation t]
designAuts d = incidenceAuts $ incidenceGraph d

-- We find design auts by finding graph auts of the incidence graph of the design
-- In a square design, we need to watch out for graph auts which are mapping points <-> blocks
designAuts1 d = filter (/=1) $ map points $ graphAuts $ incidenceGraph d where
    points h = fromPairs [(x,y) | (Left x, Left y) <- toPairs h]
     -- This implicitly filters out (Right x, Right y) action on blocks,
     -- and also (Left x, Right y) auts taking points to blocks.
     -- The filter (/=1) is to remove points <-> blocks auts

-- The incidence graph is a bipartite graph, so the distance function naturally partitions points from blocks


-- MATHIEU GROUPS AND WITT DESIGNS

alphaL2_23 = p [[-1],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]]                      -- t -> t+1
betaL2_23  = p [[-1],[0],[1,2,4,8,16,9,18,13,3,6,12],[5,10,20,17,11,22,21,19,15,7,14]]                  -- t -> 2*t
gammaL2_23 = p [[-1,0],[1,22],[2,11],[3,15],[4,17],[5,9],[6,19],[7,13],[8,20],[10,16],[12,21],[14,18]]  -- t -> -1/t

l2_23 = [alphaL2_23, betaL2_23, gammaL2_23]

-- Mathieu group M24
-- Conway and Sloane p274ff
-- This is the automorphism group of the extended binary Golay code G24
-- or alternatively of the unique Steiner system S(5,8,24) (which consists of the weight 8 codewords of the above)

deltaM24 = p [[-1],[0],[1,18,4,2,6],[3],[5,21,20,10,7],[8,16,13,9,12],[11,19,22,14,17],[15]]
-- this is t -> t^3 / 9 (for t a quadratic residue), t -> 9 t^3 (t a non-residue)

-- |Generators for the Mathieu group M24, a finite simple group of order 244823040
m24 :: [Permutation Integer]
m24 = [alphaL2_23, betaL2_23, gammaL2_23, deltaM24]

-- |A strong generating set for the Mathieu group M24, a finite simple group of order 244823040
m24sgs :: [Permutation Integer]
m24sgs = sgs m24

-- |A strong generating set for the Mathieu group M23, a finite simple group of order 10200960
m23sgs :: [Permutation Integer]
m23sgs = filter (\g -> (-1).^g == -1) m24sgs

-- |A strong generating set for the Mathieu group M22, a finite simple group of order 443520
m22sgs :: [Permutation Integer]
m22sgs = filter (\g -> 0.^g == 0) m23sgs

-- sgs uses the base implied by the Ord instance, which will be [-1,0,..]


-- Steiner system S(5,8,24)

octad = [0,1,2,3,4,7,10,12]
-- Conway&Sloane p276 - this is a weight 8 codeword from Golay code G24

-- |The Steiner system S(5,8,24), with 759 blocks, whose automorphism group is M24
s_5_8_24 :: Design Integer
s_5_8_24 = design ([-1..22], octad -^^ l2_23)
-- S(5,8,24) constructed as the image of a single octad under the action of PSL(2,23)
-- 759 blocks ( (24 `choose` 5) `div` (8 `choose` 5) )
-- Automorphism group is M24

-- |The Steiner system S(4,7,23), with 253 blocks, whose automorphism group is M23
s_4_7_23 :: Design Integer
s_4_7_23 = derivedDesign s_5_8_24 (-1)
-- 253 blocks ( (23 `choose` 4) `div` (7 `choose` 4) )
-- Automorphism group is M23

-- |The Steiner system S(3,6,22), with 77 blocks, whose automorphism group is M22
s_3_6_22 :: Design Integer
s_3_6_22 = derivedDesign s_4_7_23 0
-- 77 blocks
-- Automorphism group is M22

-- Derived design of s_3_6_22 is PG(2,F4)


-- An alternative construction
s_5_8_24' = D xs bs where
    xs = [1..24]
    bs = sift [] (combinationsOf 8 xs)
    sift ls (r:rs) = if all ((<=4) . length) [r `intersect` l | l <- ls]
                     then r : sift (r:ls) rs 
                     else sift ls rs
    sift ls [] = []


-- Could test that m22sgs are all designAuts of s_3_6_22


-- S(5,6,12) and M12

alphaL2_11 = p [[-1],[0,1,2,3,4,5,6,7,8,9,10]]          -- t -> t+1
betaL2_11  = p [[-1],[0],[1,3,9,5,4],[2,6,7,10,8]]      -- t -> 3*t
gammaL2_11 = p [[-1,0],[1,10],[2,5],[3,7],[4,8],[6,9]]  -- t -> -1/t

l2_11 = [alphaL2_11, betaL2_11, gammaL2_11]

deltaM12 = p [[-1],[0],[1],[2,10],[3,4],[5,9],[6,7],[8]]
-- Conway&Sloane p271, 327

hexad = [0,1,3,4,5,9]
-- the squares (quadratic residues) in F11
-- http://en.wikipedia.org/wiki/Steiner_system

-- |The Steiner system S(5,6,12), with 132 blocks, whose automorphism group is M12
s_5_6_12 :: Design Integer
s_5_6_12 = design ([-1..10], hexad -^^ l2_11)
-- S(5,6,12) constructed as the image of a single hexad under the action of PSL(2,11)
-- 132 blocks ( (12 `choose` 5) `div` (6 `choose` 5) )
-- Automorphism group is M12

-- |The Steiner system S(4,5,11), with 66 blocks, whose automorphism group is M11
s_4_5_11 :: Design Integer
s_4_5_11 = derivedDesign s_5_6_12 (-1)
-- 66 blocks
-- Automorphism group is M11

-- |Generators for the Mathieu group M12, a finite simple group of order 95040
m12 :: [Permutation Integer]
m12 = [alphaL2_11, betaL2_11, gammaL2_11, deltaM12]

-- |A strong generating set for the Mathieu group M12, a finite simple group of order 95040
m12sgs :: [Permutation Integer]
m12sgs = sgs m12
-- order 95040

-- |A strong generating set for the Mathieu group M11, a finite simple group of order 7920
m11sgs :: [Permutation Integer]
m11sgs = filter (\g -> (-1).^g == -1) m12sgs
-- order 7920


{-
-- WITT DESIGNS
-- S(5,8,24) AND S(5,6,12)

-- Let D be a square 2-design.
-- An n-arc is a set of n points of D, no three of which are contained in a block
arcs n (D xs bs) = map reverse $ dfs n [] xs where
    dfs 0 ys _ = [ys]
    dfs i ys xs = concat [dfs (i-1) (x:ys) (dropWhile (<=x) xs) | x <- xs, isCompatible (x:ys)]
    isCompatible ys = all ((<=2) . length) [ys `L.intersect` b | b <- bs]

tangents (D xs bs) arc = [b | b <- bs, length (arc `L.intersect` b) == 1]

-- !! NOT QUITE AS EXPECTED
-- Cameron van Lint implies that ovals should have n = 1+(k-1)/lambda, whereas I'm finding that they're one bigger than that
-- eg length $ ovals $ ag2 f3 should be 54
-- But ag2 f3 isn't a *square* design
ovals d =
    let Just (_,k,lambda) = tDesignParams 2 d
        (q,r) = (k-1) `quotRem` lambda
        n = 2+q -- == 1+(k-1)/lambda
    in if r == 0
       then [arc | arc <- arcs n d, arc == L.sort (concat $ map (L.intersect arc) $ tangents d arc)] -- each point has a unique tangent
       else []

hyperovals d =
    let Just (_,k,lambda) = tDesignParams 2 d
        (q,r) = k `quotRem` lambda
        n = 1+q -- == 1+k/lambda
    in if r == 0
       then filter (null . tangents d) $ arcs n d
       else []

-- Cameron & van Lint, p22
-- s_5_8_24 = [length (intersect (head h) (head s)) | h <- [h1,h2,h3], s <- [s1,s2,s3]] where
s_5_8_24 = design (points,lines) where
    points = map Left xs ++ map Right [1,2,3]
    lines = [map Left b ++ map Right [1,2,3] | b <- bs] ++ -- line plus three points at infinity
            [map Left h ++ map Right [2,3] | h <- h1] ++ -- hyperoval plus two points at infinity
            [map Left h ++ map Right [1,3] | h <- h2] ++
            [map Left h ++ map Right [1,2] | h <- h3] ++
            [map Left s ++ map Right [1] | s <- s1] ++ -- Baer subplanes plus one point at infinity
            [map Left s ++ map Right [2] | s <- s2] ++
            [map Left s ++ map Right [3] | s <- s3] ++
            [map Left (l1 `symDiff` l2) | l1 <- bs, l2 <- bs, l1 < l2]
    d@(D xs bs) = pg2 f4
    hs = hyperovals d
    [h1,h2,h3] = evenClasses hs
    [s2,s1,s3] = oddClasses baerSubplanes -- we have to number the ss so that if h <- hi, s <- sj, then |h intersect s| is even <=> i == j
    evenClasses (h:hs) = let (ys,ns) = partition (even . length . L.intersect h) hs in (h:ys) : evenClasses ns
    evenClasses [] = []
    oddClasses (h:hs) = let (ys,ns) = partition (odd . length . L.intersect h) hs in (h:ys) : oddClasses ns
    oddClasses [] = []
    baerSubplanes = [s | s <- baerSubplanes', and [length (L.intersect s b) `elem` [1,3] | b <- bs] ] 
    baerSubplanes' = map reverse $ dfs 7 [] xs where
        dfs 0 ys _ = [ys]
        dfs i ys xs = concat [dfs (i-1) (x:ys) (dropWhile (<=x) xs) | x <- xs, isCompatible (x:ys)]
        isCompatible ys = all ((<=3) . length) [ys `L.intersect` b | b <- bs]
-}