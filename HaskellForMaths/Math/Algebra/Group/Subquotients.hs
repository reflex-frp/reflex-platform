-- Copyright (c) David Amos, 2009. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}
-- Because unRight defined point-free

module Math.Algebra.Group.Subquotients where

import qualified Data.List as L
import qualified Data.Map as M

import Math.Common.ListSet
import Math.Algebra.Group.PermutationGroup hiding (ptStab, normalClosure)
import Math.Algebra.Group.SchreierSims (cosetRepsGx)
import Math.Algebra.Group.RandomSchreierSims


-- Source: Seress, Permutation Group Algorithms


isLeft (Left _) = True
isLeft (Right _) = False

isRight (Right _) = True
isRight (Left _) = False


unRight = fromPairs . map (\(Right a, Right b) -> (a,b)) . toPairs

restrictLeft g = fromPairs [(a,b) | (Left a, Left b) <- toPairs g]
-- note that this is doing a filter - taking only the left part of the action - and a map, unLefting


-- pointwise stabiliser of xs
ptStab gs delta = map unRight $ dropWhile (isLeft . minsupp) $ sgs gs' where
    gs' = [ (fromPairs . map (\(a,b) -> (lr a, lr b)) . toPairs) g | g <- gs]
    lr x = if x `elem` delta then Left x else Right x


{-
-- !! NEXT TWO FUNCTIONS NOT TESTED
-- Need some meaningful examples of homomorphisms
-- eg Sn -> Sym(k-subsets of n)
-- restrict to a transitive constituent
-- blocks

-- Given generators gs for a group G, and f : G -> H a homomorphism,
-- return the "semi-diagonal" subgroup [(f g, g) | g <- gs] of f(G) * G
homomorphismConstruction :: (Ord a, Ord b) => [Permutation a] -> (Permutation a -> Permutation b) -> [Permutation (Either b a)]
homomorphismConstruction gs f = [lift g | g <- gs] where
    lift g = fromPairs $ [(Right x, Right y) | (x,y) <- toPairs g] ++ [(Left x', Left y') | (x',y') <- toPairs (f g)] 

ker gs f = ks where
    gbar = homomorphismConstruction gs f
    gs' = sgs gbar
    ks' = dropWhile (\h -> isLeft $ minsupp h) gs' -- !! should filter isRight - sgs might not be in order
    ks = map unRight ks'
    unRight = fromPairs . map (\(Right a, Right b) -> (a,b)) . toPairs
-}


isTransitive :: (Ord t) => [Permutation t] -> Bool
isTransitive gs = length (orbits gs) == 1


-- TRANSITIVE CONSTITUENTS

{-
-- find largest composition factor of a group which is not transitive
-- we do this by taking the smallest orbit delta,
-- then constructing the homomorphism G -> Sym(delta)
-- and returning the kernel and the image
factorNotTransitive gs = transitiveConstituentHomomorphism' gs delta where
    delta = smallest $ orbits gs
    sizeSorted lists = map snd $ L.sort $ [(length l, l) | l <- lists]
    smallest = head . sizeSorted
-}

-- Seress p81
-- |Given a group gs and a transitive constituent ys, return the kernel and image of the transitive constituent homomorphism.
-- That is, suppose that gs acts on a set xs, and ys is a subset of xs on which gs acts transitively.
-- Then the transitive constituent homomorphism is the restriction of the action of gs to an action on the ys.
transitiveConstituentHomomorphism
  :: (Ord a, Show a) =>
     [Permutation a] -> [a] -> ([Permutation a], [Permutation a])
transitiveConstituentHomomorphism gs delta
    | delta == closure delta [(.^ g) | g <- gs] -- delta is closed under action of gs, hence a union of orbits
        = transitiveConstituentHomomorphism' gs delta

transitiveConstituentHomomorphism' gs delta = (ker, im) where
    gs' = sgs $ map (fromPairs . map (\(a,b) -> (lr a, lr b)) . toPairs) gs
    -- as delta is a transitive constituent, we will always have a and b either both Left or both Right
    lr x = if x `elem` delta then Left x else Right x
    ker = map unRight $ dropWhile (isLeft . minsupp) gs' -- pointwise stabiliser of delta
    im = map restrictLeft $ takeWhile (isLeft . minsupp) gs' -- restriction of the action to delta


-- BLOCKS OF IMPRIMITIVITY

-- Holt p83ff (and also Seress p107ff)
-- Find a minimal block containing ys. ys are assumed to be sorted.
minimalBlock gs ys@(y1:yt) = minimalBlock' p yt gs where
    xs = foldl union [] $ map supp gs
    p = M.fromList $ [(yi,y1) | yi <- ys] ++ [(x,x) | x <- xs \\ ys]
    minimalBlock' p (q:qs) (h:hs) =
        let r = p M.! q         -- representative of class containing q
            k = p M.! (q .^ h)  -- rep of class (q^h)
            l = p M.! (r .^ h)  -- rep of class (r^h)
        in if k /= l -- then we need to merge the classes
           then let p' = M.map (\x -> if x == l then k else x) p
                    qs' = qs ++ [l]
                in minimalBlock' p' (q:qs') hs
           else minimalBlock' p (q:qs) hs
    minimalBlock' p (q:qs) [] = minimalBlock' p qs gs
    minimalBlock' p [] _ =
        let reps = toListSet $ M.elems p
        in L.sort [ filter (\x -> p M.! x == r) xs | r <- reps ]
-- Because the support of the permutations is not constrained to be [1..n], we have to use a map instead of an array
-- This probably affects the complexity, but isn't a problem in practice

-- |Given a transitive group gs, find all non-trivial block systems. That is, if gs act on xs,
-- find all the ways that the xs can be divided into blocks, such that the gs also have a permutation action on the blocks
blockSystems :: (Ord t) => [Permutation t] -> [[[t]]]
blockSystems gs
    | isTransitive gs = toListSet $ filter (/= [x:xs]) $ map (minimalBlock gs) [ [x,x'] | x' <- xs ]
    | otherwise = error "blockSystems: not transitive"
    where x:xs = foldl union [] $ map supp gs


-- |A more efficient version of blockSystems, if we have an sgs
blockSystemsSGS :: (Ord a) => [Permutation a] -> [[[a]]]
blockSystemsSGS gs = toListSet $ filter (/= [x:xs]) $ map (minimalBlock gs) [ [x,x'] | x' <- rs ]
    where x:xs = foldl union [] $ map supp gs
          hs = filter (\g -> x < minsupp g) gs -- sgs for stabiliser Gx
          os = orbits hs
          rs = map head os ++ (xs \\ L.sort (concat os)) -- orbit representatives, including singleton cycles
-- Perhaps we could have a function which just returns orbit reps for stabiliser

-- eg for D 10, the stabiliser of 1 is [[2,6],[3,5]] - we need to make sure we don't forget 4

-- If we didn't have an SGS, we could try to randomly generate a few elts of stabiliser Gx, as that would still be better than nothing
-- see Holt RandomStab function


-- |A permutation group is primitive if it has no non-trivial block systems
isPrimitive :: (Ord t) => [Permutation t] -> Bool
isPrimitive gs = null (blockSystems gs)

isPrimitiveSGS :: (Ord a) => [Permutation a] -> Bool
isPrimitiveSGS gs = null (blockSystemsSGS gs)

-- There are other optimisations we haven't done
-- see Holt p86

-- |Given a transitive group gs, and a block system for gs, return the kernel and image of the block homomorphism
-- (the homomorphism onto the action of gs on the blocks)
blockHomomorphism
  :: (Ord t, Show t) =>
     [Permutation t] -> [[t]] -> ([Permutation t], [Permutation [t]])
blockHomomorphism gs bs
    | bs == closure bs [(-^ g) | g <- gs] -- bs is closed under action of gs
        = blockHomomorphism' gs bs

blockHomomorphism' gs bs = (ker,im) where
    gs' = sgs $ map lr gs
    lr g = fromPairs $ [(Left b, Left $ b -^ g) | b <- bs] ++ [(Right x, Right y) | (x,y) <- toPairs g]
    ker = map unRight $ dropWhile (isLeft . minsupp) gs' -- stabiliser of the blocks
    im = map restrictLeft $ takeWhile (isLeft . minsupp) gs' -- restriction to the action on blocks

-- Note that there is a slightly more efficient way to calculate block homomorphism,
-- but requires change of base algorithm which we haven't implemented yet


-- NORMAL CLOSURE

-- Seress 115
-- Given G, H < Sym(Omega) return <H^G> (the normal closure)
normalClosure gs hs = map unRight $ dropWhile (isLeft . minsupp) $ sgs ks where
    xs = foldl union [] $ map supp $ gs ++ hs
    ds = map diag gs -- {(g,g) | g <- G}
    diag g = fromPairs $ concat [ [(Left x, Left y) , (Right x, Right y)] | (x,y) <- toPairs g]
    hsR = map inR hs -- {(1,h) | h <- H}
    inR h = fromPairs [(Right x, Right y) | (x,y) <- toPairs h]
    ks = ds ++ hsR

-- Seress 116
-- Given G, H < Sym(Omega) return <H^G> `intersection` G
intersectionNormalClosure gs hs = map unRight $ dropWhile (isLeft . minsupp) $ sgs ks where
    xs = foldl union [] $ map supp $ gs ++ hs
    ds = map diag gs -- {(g,g) | g <- G}
    diag g = fromPairs $ concat [ [(Left x, Left y) , (Right x, Right y)] | (x,y) <- toPairs g]
    hsL = map inL hs -- {(h,1) | h <- H}
    inL h = fromPairs [(Left x, Left y) | (x,y) <- toPairs h]
    ks = ds ++ hsL


-- CENTRALISER IN THE SYMMETRIC GROUP

-- Centralizer of G in Sym(X) - transitive case
centralizerSymTrans gs = filter (/= 1) $ centralizerSymTrans' [] fix_g_a where
    xs@(a:_) = foldl union [] $ map supp gs
    ss = sgs gs
    g_a = dropWhile ( (==a) . minsupp ) ss -- pt stabiliser of a
    fix_g_a = xs \\ (foldl union [] $ map supp g_a) -- the pts fixed by stabiliser of a
    reps_a = cosetRepsGx gs a
    -- xs = M.keys reps_a
    centralizingElt b = fromPairs [ let g = reps_a M.! x in (x, b .^ g) | x <- xs ]
    centralizerSymTrans' ls (r:rs) =
        let c = centralizingElt r
        in c : centralizerSymTrans' (c:ls) (rs \\ orbitP (c:ls) a)
    centralizerSymTrans' _ [] = []

