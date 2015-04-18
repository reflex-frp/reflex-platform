-- Copyright (c) David Amos, 2008-2012. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}

-- |A module for doing arithmetic in permutation groups.
--
-- Group elements are represented as permutations of underlying sets, and are entered and displayed
-- using a Haskell-friendly version of cycle notation. For example, the permutation (1 2 3)(4 5)
-- would be entered as @p [[1,2,3],[4,5]]@, and displayed as [[1,2,3],[4,5]]. Permutations can be defined
-- over arbitrary underlying sets (types), not just the integers.
--
-- If @g@ and @h@ are group elements, then the expressions @g*h@ and @g^-1@ calculate product and inverse respectively.
module Math.Algebra.Group.PermutationGroup where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Math.Common.ListSet (toListSet, union, (\\) ) -- a version of union which assumes the arguments are ascending sets (no repeated elements)

import Math.Core.Utils hiding (elts)
import Math.Algebra.LinearAlgebra hiding (inverse) -- only needed for use in ghci

infix 8 ~^

rotateL (x:xs) = xs ++ [x]


-- PERMUTATIONS

-- |A type for permutations, considered as functions or actions which can be performed on an underlying set.
newtype Permutation a = P (M.Map a a) deriving (Eq,Ord)

-- |Construct a permutation from a list of cycles.
-- For example, @p [[1,2,3],[4,5]]@ returns the permutation that sends 1 to 2, 2 to 3, 3 to 1, 4 to 5, 5 to 4.
p :: (Ord a) => [[a]] -> Permutation a
p = fromCycles

fromPairs xys | isValid   = fromPairs' xys
              | otherwise = error "Not a permutation"
    where (xs,ys) = unzip xys
          (xs',ys') = (L.sort xs, L.sort ys)
          isValid = xs' == ys' && all ((==1) . length) (L.group xs') -- ie the domain and range are the same, and are *sets*

fromPairs' xys = P $ M.fromList $ filter (uncurry (/=)) xys
-- we remove fixed points, so that the derived Eq instance works as expected

toPairs (P g) = M.toList g

fromList xs = fromPairs $ zip xs (L.sort xs)
-- for example, fromList [2,3,1] is [[1,3,2]] - because the 1 moved to the 3 position

-- the support of a permutation is the points it moves (returned in ascending order)
supp (P g) = M.keys g
-- (This is guaranteed not to contain fixed points provided the permutations have been constructed using the supplied constructors)

-- |x .^ g returns the image of a vertex or point x under the action of the permutation g.
-- For example, @1 .^ p [[1,2,3]]@ returns 2.
-- The dot is meant to be a mnemonic for point or vertex.
(.^) :: (Ord a) => a -> Permutation a -> a
x .^ P g = case M.lookup x g of
           Just y  -> y
           Nothing -> x -- if x `notElem` supp (P g), then x is not moved

-- |b -^ g returns the image of an edge or block b under the action of the permutation g.
-- For example, @[1,2] -^ p [[1,4],[2,3]]@ returns [3,4].
-- The dash is meant to be a mnemonic for edge or line or block.
(-^) :: (Ord a) => [a] -> Permutation a -> [a]
xs -^ g = L.sort [x .^ g | x <- xs]

-- construct a permutation from cycles
fromCycles cs = fromPairs $ concatMap fromCycle cs
    where fromCycle xs = zip xs (rotateL xs)

-- convert a permutation to cycles
toCycles g = toCycles' $ supp g
    where toCycles' ys@(y:_) = let c = cycleOf g y in c : toCycles' (ys L.\\ c)
          toCycles' [] = []

cycleOf g x = cycleOf' x [] where
    cycleOf' y ys = let y' = y .^ g in if y' == x then reverse (y:ys) else cycleOf' y' (y:ys)

instance (Ord a, Show a) => Show (Permutation a) where
    show g | g == 1 = "1"
           | otherwise = show (toCycles g)

parity g = let cs = toCycles g in (length (concat cs) - length cs) `mod` 2
-- parity' g = length (filter (even . length) $ toCycles g) `mod` 2

sign g = (-1)^(parity g)

orderElt g = foldl lcm 1 $ map length $ toCycles g
-- == order [g]

-- |The Num instance is what enables us to write @g*h@ for the product of group elements and @1@ for the group identity.
-- Unfortunately we can't of course give sensible definitions for the other functions declared in the Num typeclass.
instance (Ord a, Show a) => Num (Permutation a) where
    g * h = fromPairs' [(x, x .^ g .^ h) | x <- supp g `union` supp h]
    -- signum = sign -- doesn't work, complains about no (+) instance
    fromInteger 1 = P $ M.empty
    _ + _ = error "(Permutation a).+: not applicable"
    negate _ = error "(Permutation a).negate: not applicable"
    abs _ = error "(Permutation a).abs: not applicable"
    signum _ = error "(Permutation a).signum: not applicable"

-- |The HasInverses instance is what enables us to write @g^-1@ for the inverse of a group element.
instance (Ord a, Show a) => HasInverses (Permutation a) where
    inverse (P g) = P $ M.fromList $ map (\(x,y)->(y,x)) $ M.toList g


-- |g ~^ h returns the conjugate of g by h, that is, h^-1*g*h.
-- The tilde is meant to a mnemonic, because conjugacy is an equivalence relation.
(~^) :: (Ord a, Show a) => Permutation a -> Permutation a -> Permutation a
g ~^ h = h^-1 * g * h

-- commutator
comm g h = g^-1 * h^-1 * g * h


-- ORBITS

closureS xs fs = closure' S.empty (S.fromList xs) where
    closure' interior boundary
        | S.null boundary = interior
        | otherwise =
            let interior' = S.union interior boundary
                boundary' = S.fromList [f x | x <- S.toList boundary, f <- fs] S.\\ interior'
            in closure' interior' boundary'

closure xs fs = S.toList $ closureS xs fs

orbit action x gs = closure [x] [ (`action` g) | g <- gs]

-- |x .^^ gs returns the orbit of the point or vertex x under the action of the gs
(.^^) :: (Ord a) => a -> [Permutation a] -> [a]
x .^^ gs = orbit (.^) x gs

orbitP gs x = orbit (.^) x gs
orbitV gs x = orbit (.^) x gs

-- |b -^^ gs returns the orbit of the block or edge b under the action of the gs
(-^^) :: (Ord a) => [a] -> [Permutation a] -> [[a]]
b -^^ gs = orbit (-^) b gs

orbitB gs b = orbit (-^) b gs
orbitE gs b = orbit (-^) b gs


action xs f = fromPairs [(x, f x) | x <- xs]


-- find all the orbits of a group
-- (as we typically work with transitive groups, this is more useful for studying induced actions)
-- (Note that of course this won't find orbits of points which are fixed by all elts of G)
orbits gs = let xs = foldl union [] $ map supp gs in orbits' xs
    where orbits' [] = []
          orbits' (x:xs) = let o = x .^^ gs in o : orbits' (xs L.\\ o)


-- GROUPS
-- Some standard sequences of groups, and constructions of new groups from old

-- |_C n returns generators for Cn, the cyclic group of order n
_C :: (Integral a) => a -> [Permutation a]
_C n | n >= 2 = [p [[1..n]]]

-- D2n, dihedral group of order 2n, symmetry group of n-gon
-- For example, _D 8 == _D2 4 == symmetry group of square
_D n | r == 0 = _D2 q where (q,r) = n `quotRem` 2

_D2 n | n >= 3 = [a,b] where
    a = p [[1..n]]                            -- rotation
    b = p [[i,n+1-i] | i <- [1..n `div` 2]]   -- reflection
    -- b = fromPairs $ [(i,n+1-i) | i <- [1..n]] -- reflection

-- |_S n returns generators for Sn, the symmetric group on [1..n]
_S :: (Integral a) => a -> [Permutation a]
_S n | n >= 3 = [s,t]
     | n == 2 = [t]
     | n == 1 = []
    where s = p [[1..n]]
          t = p [[1,2]]

-- |_A n returns generators for An, the alternating group on [1..n]
_A :: (Integral a) => a -> [Permutation a]
_A n | n > 3 = [s,t]
     | n == 3 = [t]
     | n == 2 = []
    where s | odd n  = p [[3..n]]
            | even n = p [[1,2], [3..n]]
          t = p [[1,2,3]]


-- |Given generators for groups H and K, acting on sets A and B respectively,
-- return generators for the direct product H*K, acting on the disjoint union A+B (= Either A B)
dp :: (Ord a, Ord b) => [Permutation a] -> [Permutation b] -> [Permutation (Either a b)]
dp hs ks =
    [P $ M.fromList $ map (\(x,x') -> (Left x,Left x')) $ M.toList h' | P h' <- hs] ++
    [P $ M.fromList $ map (\(y,y') -> (Right y,Right y')) $ M.toList k' | P k' <- ks]

-- Wreath product of groups
-- Given generators for H and K, acting on sets X and Y respectively,
-- return generators for H wr K, acting on X*Y (== (X,Y))
-- (Cameron, Combinatorics, p229-230; Cameron, Permutation Groups, p11-12)
wr hs ks =
    let _X = S.toList $ foldl S.union S.empty [M.keysSet h' | P h' <- hs] -- set on which H acts
        _Y = S.toList $ foldl S.union S.empty [M.keysSet k' | P k' <- ks] -- set on which K acts
        -- Then the wreath product acts on cartesian product X * Y,
        -- regarded as a fibre bundle over Y of isomorphic copies of X
        _B = [P $ M.fromList $ map (\(x,x') -> ((x,y),(x',y))) $ M.toList h' | P h' <- hs, y <- _Y]
        -- bottom group B applies the action of H within each fibre
        _T = [P $ M.fromList [((x,y),(x,y')) | x <- _X, (y,y') <- M.toList k'] | P k' <- ks]
        -- top group T uses the action of K to permute the fibres
    in _B ++ _T -- semi-direct product of B and T
-- !! Why using M.keysSet rather than supp?

-- embed group elts into Sn - ie, convert so that the set acted on is [1..n]
toSn gs = [toSn' g | g <- gs] where
    _X = L.sort $ foldl union [] $ map supp gs   -- the set on which G acts
    mapping = M.fromList $ zip _X [1..] -- the mapping from _X to [1..n]
    toSn' g = fromPairs' $ map (\(x,x') -> (mapping M.! x, mapping M.! x')) $ toPairs g

-- Given a permutation over lists of small positive integers, such as [1,2,3],
-- return a permutation over the integers obtained by interpreting the lists as digits.
-- For example, [1,2,3] -> 123.
fromDigits g = fromPairs [(fromDigits' x, fromDigits' y) | (x,y) <- toPairs g]

fromDigits' xs = f (reverse xs) where
    f (x:xs) = x + 10 * f xs
    f [] = 0

-- Given a permutation over lists of 0s and 1s,
-- return the permutation obtained by interpreting these as binary digits.
-- For example, [1,1,0] -> 6.
fromBinary g = fromPairs [(fromBinary' x, fromBinary' y) | (x,y) <- toPairs g]

fromBinary' xs = f (reverse xs) where
    f (x:xs) = x + 2 * f xs
    f [] = 0




-- INVESTIGATING GROUPS
-- Functions to investigate groups in various ways
-- Most of these functions will only be efficient for small groups (say |G| < 10000)
-- For larger groups we will need to use Schreier-Sims and associated algorithms

-- |Given generators for a group, return a (sorted) list of all elements of the group.
-- Implemented using a naive closure algorithm, so only suitable for small groups (|G| < 10000)
elts :: (Num a, Ord a) => [a] -> [a]
elts gs = closure [1] [ (*g) | g <- gs]

eltsS gs = closureS [1] [ (*g) | g <- gs]

-- |Given generators for a group, return the order of the group (the number of elements).
-- Implemented using a naive closure algorithm, so only suitable for small groups (|G| < 10000)
order :: (Num a, Ord a) => [a] -> Int
order gs = S.size $ eltsS gs -- length $ elts gs

isMember gs h = h `S.member` eltsS gs -- h `elem` elts gs


-- TRANSVERSAL GENERATING SETS
-- The functions graphAuts2 and graphAuts3 return generating sets consisting of successive transversals
-- In this case, we don't need to run Schreier-Sims to list elements or calculate order

minsupp = head . supp

-- calculate the order of the group, given a "transversal generating set"
orderTGS tgs =
    let transversals = map (1:) $ L.groupBy (\g h -> minsupp g == minsupp h) tgs
    in product $ map L.genericLength transversals

-- list the elts of the group, given a "transversal generating set"
eltsTGS tgs =
    let transversals = map (1:) $ L.groupBy (\g h -> minsupp g == minsupp h) tgs
    in map product $ sequence transversals

-- recover a transversal generating set from a strong generating set
-- A strong generating set is a generating set gs such that <gs intersect si> = si
-- ie, its intersection with each successive stabiliser in the chain generates the stabiliser
tgsFromSgs sgs = concatMap transversal bs where
    bs = toListSet $ map minsupp sgs
    transversal b = closure b $ filter ( (b <=) . minsupp ) sgs
    closure b gs = closure' M.empty (M.fromList [(b, 1)]) where
        closure' interior boundary
            | M.null boundary = filter (/=1) $ M.elems interior
            | otherwise =
                let interior' = M.union interior boundary
                    boundary' = M.fromList [(x .^ g, h*g) | (x,h) <- M.toList boundary, g <- gs] M.\\ interior'
                in closure' interior' boundary'
-- For example, sgs (_A 5) == [[[1,2,3]],[[2,4,5]],[[3,4,5]]]
-- So we need all three to generate the first transversal, then the last two to generate the second transversal, etc

-- |Given a strong generating set, return the order of the group it generates
orderSGS :: (Ord a) => [Permutation a] -> Integer
orderSGS sgs = product $ map (L.genericLength . fundamentalOrbit) bs where
    bs = toListSet $ map minsupp sgs
    fundamentalOrbit b = b .^^ filter ( (b <=) . minsupp ) sgs


-- MORE INVESTIGATIONS

-- given the elts of a group, find generators
gens hs = gens' [] (S.singleton 1) hs where
    gens' gs eltsG (h:hs) = if h `S.member` eltsG then gens' gs eltsG hs else gens' (h:gs) (eltsS $ h:gs) hs
    gens' gs _ [] = reverse gs



-- conjClass gs h = orbit (~^) gs h

-- Conjugacy class - should only be used for small groups
h ~^^ gs = conjClass gs h

conjClass gs h = closure [h] [ (~^ g) | g <- gs]
-- conjClass gs h = h ~^^ gs

-- |conjClassReps gs returns conjugacy class representatives and sizes for the group generated by gs.
-- This implementation is only suitable for use with small groups (|G| < 10000).
conjClassReps :: (Ord a, Show a) => [Permutation a] -> [(Permutation a, Int)]
conjClassReps gs = conjClassReps' (elts gs) where
    conjClassReps' (h:hs) =
        let cc = conjClass gs h in (h, length cc) : conjClassReps' (hs \\ cc)
    conjClassReps' [] = []
-- using the ListSet implementation of \\, since we know both lists are sorted

{-
-- This is just the orbits under conjugation. Can we generalise "orbits" to help us here?
conjClasses gs = conjClasses' (elts gs)
    where conjClasses' [] = []
          conjClasses' (h:hs) = let c = conjClass gs h in c : conjClasses' (hs L.\\ c)
-}


-- given list of generators, try to find a shorter list
reduceGens (1:gs) = reduceGens gs
reduceGens (g:gs) = reduceGens' ([g], eltsS [g]) gs where
    reduceGens' (gs,eltsgs) (h:hs) =
        if h `S.member` eltsgs
        then reduceGens' (gs,eltsgs) hs
        else reduceGens' (h:gs, eltsS $ h:gs) hs
    reduceGens' (gs,_) [] = reverse gs


-- SUBGROUPS

isSubgp hs gs = all (`S.member` gs') hs
    where gs' = eltsS gs

-- The following is similar to the "cyclic extension" method - Holt p385
-- However, Holt only looks at normal cyclic extensions (ie, by an elt of prime order), and so only finds solvable subgps

-- |Return the subgroups of a group. Only suitable for use on small groups (eg < 100 elts)
subgps :: (Ord a, Show a) => [Permutation a] -> [[Permutation a]]
subgps gs = [] : subgps' S.empty [] (map (:[]) hs) where
    hs = filter isMinimal $ elts gs
    subgps' found ls (r:rs) =
        let ks = elts r in
        if ks `S.member` found
        then subgps' found ls rs
        else r : subgps' (S.insert ks found) (r:ls) rs
    subgps' found [] [] = []
    subgps' found ls [] = subgps' found [] [l ++ [h] | l <- reverse ls, h <- hs, last l < h]

-- g is the minimal elt in the cyclic subgp it generates
isMinimal 1 = False
isMinimal g = all (g <=) primitives -- g == minimum primitives
    where powers = takeWhile (/=1) $ tail $ iterate (*g) 1
          n = orderElt g -- == length powers + 1
          primitives = filter (\h -> orderElt h == n) powers


-- centralizer of a subgroup or a set of elts
-- the centralizer of H in G is the set of elts of G which commute with all elts of H
centralizer gs hs = [k | k <- elts gs, all (\h -> h*k == k*h) hs]

-- the centre of G is the set of elts of G which commute with all other elts
centre gs = centralizer gs gs

-- normaliser of a subgroup
-- the normaliser of H in G is {g <- G | g^-1Hg == H}
-- it is a subgroup of G, and H is a normal subgroup of it: H <|= N_G(H) <= G
normalizer gs hs = [g | g <- elts gs, all (\h -> h~^g `elem` elts hs) hs]

-- stabilizer of a point
stabilizer gs x = [g | g <- elts gs, x .^ g == x]

-- pointwise stabiliser of a set
ptStab gs xs = [g | g <- elts gs, and [x .^ g == x | x <- xs] ]

-- setwise stabiliser of a set
setStab gs xs = [g | g <- elts gs, xs -^ g == xs]

-- normal closure of H in G
normalClosure gs hs = reduceGens $ hs ++ [h ~^ g | h <- hs, g <- gs ++ map inverse gs]

-- commutator gp of H and K
commutatorGp hs ks = normalClosure (hsks) [h^-1 * k^-1 * h * k | h <- hs', k <- ks']
    where hs' = reduceGens hs
          ks' = reduceGens ks
          hsks = reduceGens (hs' ++ ks')
          -- no point processing more potential generators than we have to

-- derived subgroup
derivedSubgp gs = commutatorGp gs gs


-- ACTION ON COSETS, QUOTIENT GROUPS

xs -*- ys = toListSet [x*y | x <- xs, y <- ys]

xs -*  y  = L.sort [x*y | x <- xs] -- == xs -*- [y]
x   *- ys = L.sort [x*y | y <- ys] -- == [x] -*- ys

-- |isNormal gs ks returns True if \<ks\> is normal in \<gs\>.
-- Note, it is caller's responsibility to ensure that \<ks\> is a subgroup of \<gs\> (ie that each k is in \<gs\>).
isNormal :: (Ord a, Show a) => [Permutation a] -> [Permutation a] -> Bool
isNormal gs ks = all (== ks') [ (g^-1) *- ks' -* g | g <- gs]
    where ks' = elts ks

-- |Return the normal subgroups of a group. Only suitable for use on small groups (eg < 100 elts)
normalSubgps :: (Ord a, Show a) => [Permutation a] -> [[Permutation a]]
normalSubgps gs = filter (isNormal gs) (subgps gs)

isSimple gs = length (normalSubgps gs) == 2

-- Note: caller must ensure that hs is a subgp of gs
cosets gs hs = orbit (-*) hs' gs
    where hs' = elts hs

-- |quotientGp gs ks returns \<gs\> / \<ks\>
quotientGp :: (Ord a, Show a) => [Permutation a] -> [Permutation a] -> [Permutation Int]
quotientGp gs ks
    | ks `isNormal` gs = gens $ toSn [action cosetsK (-* g) | g <- gs]
    | otherwise = error "quotientGp: not well defined unless ks normal in gs"
    where cosetsK = cosets gs ks

-- |Synonym for quotientGp
(//) :: (Ord a, Show a) => [Permutation a] -> [Permutation a] -> [Permutation Int]
gs // ks = quotientGp gs ks


-- action of group element on a subset by conjugation
xs ~~^ g = L.sort [x ~^ g | x <- xs]

conjugateSubgps gs hs = orbit (~~^) hs' gs
    where hs' = elts hs
-- not necessarily transitive on isomorphic subgps - eg a gp with an outer aut

subgpAction gs hs =
    let conjugatesH = conjugateSubgps gs hs
    in toSn [action conjugatesH (~~^ g) | g <- gs]


-- in cube gp, the subgps all appear to correspond to stabilisers of subsets, or of blocks


-- right regular permutation representation
rrpr gs h = rrpr' (elts gs) h

rrpr' gs h = fromPairs [(g, g*h) | g <- gs]

permutationMatrix xs g = [ [if x .^ g == y then 1 else 0 | y <- xs] | x <- xs ]
