-- Copyright (c) David Amos, 2008. All rights reserved.

module Math.Algebra.Group.StringRewriting where

import Data.List as L
import Data.Maybe (catMaybes)

-- REWRITING

-- |Given a list of rewrite rules of the form (left,right), and a word,
-- rewrite it by repeatedly replacing any left substring in the word by the corresponding right
rewrite :: (Eq a) => [([a], [a])] -> [a] -> [a]
rewrite rules word = rewrite' rules word where
    rewrite' (r:rs) xs =
        case rewrite1 r xs of
        Nothing -> rewrite' rs xs
        Just ys -> rewrite' rules ys
    rewrite' [] xs = xs

rewrite1 (l,r) xs =
    case xs `splitSubstring` l of
    Nothing -> Nothing
    Just (a,b) -> Just (a++r++b)

-- given a string x and a substring b, find if possible (a,c) such that xs = abc
splitSubstring xs ys = splitSubstring' [] xs where
    splitSubstring' ls [] = Nothing
    splitSubstring' ls (r:rs) =
        if ys `L.isPrefixOf` (r:rs)
        then Just (reverse ls, drop (length ys) (r:rs))
        else splitSubstring' (r:ls) rs
-- there might be a more efficient way to do this


-- KNUTH-BENDIX

-- given two strings x,y, find if possible a,b,c with x=ab y=bc
findOverlap xs ys = findOverlap' [] xs ys where
    findOverlap' as [] cs = Nothing -- (reverse as, [], cs)
    findOverlap' as (b:bs) cs =
        if (b:bs) `L.isPrefixOf` cs
        then Just (reverse as, b:bs, drop (length (b:bs)) cs)
        else findOverlap' (b:as) bs cs
-- there might be a more efficient way to do this

-- note that findOverlap "abab" "abab" won't find the partial overlap ("ab","ab","ab")

-- Knuth-Bendix algorithm
-- http://en.wikipedia.org/wiki/Knuth-Bendix_algorithm
-- Given a set of rules (assumed already reduced with respect to each other)
-- return a confluent rewrite system
knuthBendix1 rules = knuthBendix' rules pairs where
    pairs = [(lri,lrj) | lri <- rules, lrj <- rules, lri /= lrj]
    knuthBendix' rules [] = rules -- should reduce in some way
    knuthBendix' rules ( ((li,ri),(lj,rj)) : ps) =
        case findOverlap li lj of
        Nothing -> knuthBendix' rules ps
        Just (a,b,c) -> case ordpair (rewrite rules (ri++c)) (rewrite rules (a++rj)) of
                        Nothing -> knuthBendix' rules ps -- they both reduce to the same thing
                        Just rule' -> let rules' = reduce rule' rules
                                          ps' = ps ++ [(rule',rule) | rule <- rules'] ++ [(rule,rule') | rule <- rules']
                                      in knuthBendix' (rule':rules') ps'
                    	-- the new rule comes from seeing that
                    	-- a ++ b ++ c == l1 ++ c -> r1 ++ c (by rule 1)
                    	-- a ++ b ++ c == a ++ l2 -> a ++ r2 (by rule 2)
    reduce rule@(l,r) rules = filter (\(l',r') -> not (L.isInfixOf l l')) rules
        -- [rule' | rule'@(l',r') <- rules, not (l `L.isInfixOf` l')]

ordpair x y =
    case shortlex x y of
    LT -> Just (y,x)
    EQ -> Nothing
    GT -> Just (x,y)

shortlex x y = compare (length x, x) (length y, y)

-- for groups, where "letters" will take the form Either a a, we will want a different order, because we will want x^-1 -> x^3 to be the right way round


-- An optimisation - keep the rules ordered smallest first, and process the pairs smallest first
-- Appears to be significantly faster on average
knuthBendix2 rules = map snd $ knuthBendix' rules' pairs where
    rules' = L.sort $ map sizedRule rules
    pairs = L.sort [sizedPair sri srj | sri <- rules', srj <- rules', sri /= srj]
    knuthBendix' rules [] = rules
    knuthBendix' rules ( (s,(li,ri),(lj,rj)) : ps) =
        case findOverlap li lj of
        Nothing -> knuthBendix' rules ps
        Just (a,b,c) -> case ordpair (rewrite (map snd rules) (ri++c)) (rewrite (map snd rules) (a++rj)) of
                        Nothing -> knuthBendix' rules ps -- they both reduce to the same thing
                        Just rule' -> let rules' = reduce (snd rule') rules
                                          -- ps' = L.sort $ ps ++ [sizedPair rule' rule | rule <- rules'] ++ [sizedPair rule rule' | rule <- rules']
                                          ps' = merge ps $ merge [sizedPair rule' rule | rule <- rules'] [sizedPair rule rule' | rule <- rules']
                                     in knuthBendix' (L.insert rule' rules') ps'
    reduce rule@(l,r) rules = filter (\(s',(l',r')) -> not (L.isInfixOf l l')) rules
    -- reduce rule@(l,r) rules = [rule' | rule'@(s',(l',r')) <- rules, not (l `L.isInfixOf` l')]
    ordpair x y =
        let lx = length x; ly = length y in
            case compare (lx,x) (ly,y) of
            LT -> Just (ly,(y,x)); EQ -> Nothing; GT -> Just (lx,(x,y))
    sizedRule (rule@(l,r)) = (length l, rule)
    sizedPair (s1,r1) (s2,r2) = (s1+s2,r1,r2)

-- merge two ordered lists
merge (x:xs) (y:ys) =
    case compare x y of
    LT -> x : merge xs (y:ys)
    GT -> y : merge (x:xs) ys
    EQ -> error "" -- shouldn't happen in our case
merge xs ys = xs++ys

-- Another optimisation - at the stage where we remove some rules, we remove corresponding pairs too
-- Seems to perform about 25% faster on large problems (eg Coxeter groups A4-12, B4-12)
knuthBendix3 rules = knuthBendix' rules' pairs (length rules' + 1) where
    rules' = L.sort $ zipWith (\i (l,r) -> (length l,i,(l,r)) ) [1..] rules
    pairs = L.sort [sizedPair ri rj | ri <- rules', rj <- rules', ri /= rj]
    knuthBendix' rules [] k = map (\(s,i,r) -> r) rules
    knuthBendix' rules ( (s,(i,j),((li,ri),(lj,rj))) : ps) k =
        case findOverlap li lj of
        Nothing -> knuthBendix' rules ps k
        Just (a,b,c) -> case ordpair k (rewrite (map third rules) (ri++c)) (rewrite (map third rules) (a++rj)) of
                        Nothing -> knuthBendix' rules ps k -- they both reduce to the same thing
                        Just rule'@(_,_,(l,r)) ->
                            let (outrules,inrules) = L.partition (\(s',i',(l',r')) -> L.isInfixOf l l') rules
                                removedIndices = map second outrules
                                ps' = [p | p@(s,(i,j),(ri,rj)) <- ps, i `notElem` removedIndices, j `notElem` removedIndices]
                                ps'' = merge ps' $ merge [sizedPair rule' rule | rule <- inrules] [sizedPair rule rule' | rule <- inrules]
                            in knuthBendix' (L.insert rule' inrules) ps'' (k+1)
    ordpair k x y =
        let lx = length x; ly = length y in
            case compare (lx,x) (ly,y) of
            LT -> Just (ly,k,(y,x)); EQ -> Nothing; GT -> Just (lx,k,(x,y))
    second (s,i,r) = i
    third (s,i,r) = r
    sizedPair (si,i,ri) (sj,j,rj) = (si+sj,(i,j),(ri,rj))


-- |Implementation of the Knuth-Bendix algorithm. Given a list of relations, return a confluent rewrite system.
-- The algorithm is not guaranteed to terminate.
knuthBendix :: (Ord a) => [([a], [a])] -> [([a], [a])]
knuthBendix relations = knuthBendix3 (reduce [] rules) where
    rules = catMaybes [ordpair x y | (x,y) <- relations]
    reduce ls (r:rs) = reduce (r: reduce' r ls) (reduce' r rs)
    reduce ls [] = ls
    reduce' r rules = catMaybes [ordpair (rewrite [r] lhs) (rewrite [r] rhs) | (lhs,rhs) <- rules]


-- |Given generators and a confluent rewrite system, return (normal forms of) all elements
nfs :: (Ord a) => ([a], [([a], [a])]) -> [[a]]
nfs (gs,rs) = nfs' [[]] where
    nfs' [] = [] -- we have run out of words - this monoid is finite
    nfs' ws = let ws' = [g:w | g <- gs, w <- ws, not (any (`L.isPrefixOf` (g:w)) (map fst rs))]
              in ws ++ nfs' ws'

-- |Given generators and relations, return (normal forms of) all elements
elts :: (Ord a) => ([a], [([a], [a])]) -> [[a]]
elts (gs,rs) = nfs (gs, knuthBendix rs)


-- PRESENTATIONS FOR SOME STANDARD GROUPS
-- Would like to add a few more to this list

newtype SGen = S Int deriving (Eq,Ord)

instance Show SGen where
    show (S i) = "s" ++ show i

s_ i = S i
s1 = s_ 1
s2 = s_ 2
s3 = s_ 3

-- D L Johnson, Presentations of Groups, p62

-- symmetric group, generated by adjacent transpositions
_S n = (gs, r ++ s ++ t) where
    gs = map s_ [1..n-1]
    r = [([s_ i, s_ i],[]) | i <- [1..n-1]]
    s = [(concat $ replicate 3 [s_ i, s_ (i+1)],[]) | i <- [1..n-2]]
    t = [([s_ i, s_ j, s_ i, s_ j],[]) | i <- [1..n-1], j <- [i+2..n-1]]

-- braid presentation for Sn
_S' n = (gs, r ++ s ++ t) where
    gs = map s_ [1..n-1]
    r = [([s_ i, s_ i], []) | i <- [1..n-1]]
    s = [([s_ (i+1), s_ i, s_ (i+1)], [s_ i, s_ (i+1), s_ i] ) | i <- [1..n-2]]
    t = [([s_ i, s_ j, s_ i, s_ j], []) | i <- [1..n-1], j <- [i+2..n-1]]

-- http://en.wikipedia.org/wiki/Triangle_group
-- triangle groups - Johnson p127ff
tri l m n = ("abc", [("aa",""),("bb",""),("cc",""),("ab" ^ l,""),("bc" ^ n,""),("ca" ^ m,"" )])
    where xs ^ i = concat $ replicate i xs

-- von Dyck groups - Johnson p121ff
-- The subgroup of index 2 in the triangle group consisting of elts that preserve the orientation of the triangle
_D l m n = ("xy", [("x" ^ l,""), ("y" ^ m,""), ("xy" ^ n,"")])
    where xs ^ i = concat $ replicate i xs

-- Degenerate cases: n == 1 => cyclic group
-- l,2,2, l>=2 -> n-gon bipyramid - dihedral group
-- Spherical case: 1/l+1/m+1/n > 1
-- 3,3,2 -> tetrahedron; 4,3,2 -> octahedron; 5,3,2 -> icosahedron
-- Euclidean case: 1/l+1/m+1/n == 1
-- 3,3,3  4,4,2  6,3,2 
-- Hyperbolic case: 1/l+1/m+1/n < 1


