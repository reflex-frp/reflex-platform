-- Copyright (c) David Amos, 2008. All rights reserved.

{-# LANGUAGE FlexibleInstances #-}

module Math.Test.TPermutationGroup where

import qualified Data.List as L

import Math.Core.Utils hiding (elts)

import Math.Algebra.Group.PermutationGroup as P
import Math.Algebra.Group.SchreierSims as SS
import Math.Algebra.Group.RandomSchreierSims as RSS
import Math.Algebra.Group.CayleyGraph
import Math.Combinatorics.Graph
import Math.Combinatorics.GraphAuts
import Math.Projects.Rubik

import Test.QuickCheck hiding (choose)

factorials = scanl (*) 1 [1..] :: [Integer]

-- factorial representation
-- express n as a sum [ai * i! | i <- [1..]]
facRep n = facRep' [] facs n where
    facs = reverse $ takeWhile (<= n) $ tail factorials -- [i!, ..., 2!, 1!] where n >= i!
    facRep' as (f:fs) n = let (q,r) = n `quotRem` f
                          in facRep' (q : as) fs r
    facRep' as [] 0     = as


-- Unrank a permutation in Sn, using lexicographic order
-- eg for S3, we have
-- r    facRep r     unrankSn 3 r
-- 0    0.1!+0.2!    [1,2,3]
-- 1    1.1!+0.2!    [1,3,2]
-- 2    0.1!+1.2!    [2,1,3]
-- 3    1.1!+1.2!    [2,3,1]
-- 4    0.1!+2.2!    [3,1,2]
-- 5    1.1!+2.2!    [3,2,1]
-- So the image of 1 is determined by the most significant digit of the facRep, and then recurse on the remaining
unrankSn n r | r < factorial (toInteger n) =
    let ds = reverse $ take (n-1) $ facRep r ++ repeat 0
    in unrank' ds [1..n]
    where unrank' (d:ds) xs = let x = xs !! fromIntegral d in x : unrank' ds (L.delete x xs)
          unrank' [] [x] = [x]

-- unrank permutations of S(N) (where N is the natural numbers from 1)
-- doesn't use lexicographic order any more, but still a 1-1 mapping from N to permutations
unrankSN r | r >= 0 = let ds = reverse (facRep r) in reverse (unrank' ds [1..length ds + 1]) where
    unrank' (d:ds) xs = let x = if d==0 then last xs else xs !! (fromIntegral d-1)
                        in x : unrank' ds (L.delete x xs)
    unrank' [] [x] = [x]

-- perm r = fromPairs $ zip [1..] $ unrankSN r

instance Arbitrary (Permutation Int) where
    arbitrary = do r <- arbitrary -- :: Gen Integer
                   return (fromList $ unrankSN $ abs r)
                   -- return (perm (abs r))
                   -- return (perm (r^2)) -- to get some larger perms
    -- coarbitrary = undefined


prop_Group (g,h,k) =
    g*(h*k)==(g*h)*k            && -- associativity
    1*g == g && g*1 == g        && -- identity
    g*(g^-1) == 1 && (g^-1)*g == 1 -- inverse

prop_GroupPerm (g,h,k) = prop_Group (g,h,k)
    where types = (g,h,k) :: (Permutation Int, Permutation Int, Permutation Int)




prop_Transpositions g =
    g == (fromTranspositions . toTranspositions) g
    where types = g :: Permutation Int


-- Could do more, like taking arbitrary lists of perms as generators of a group,
-- and checking that the centre has the required property, etc


factorial n = product [1..n]

choose n m | m <= n = product [m+1..n] `div` product [1..n-m]

test = and [sgsTest, ssTest, ccTest, rubikTest]


sgsTest = all (uncurry (==)) sgsTests

sgsTests =
    [(sgsOrder $ _A n, SS.order $ _A n) | n <- [4..7] ] ++
    [(sgsOrder $ _S n, SS.order $ _S n) | n <- [4..7] ] ++
    [(sgsOrder $ _D2 n, SS.order $ _D2 n) | n <- [4..10] ] ++
    [let _G = toSn (_S 3 `dp` _S 3) in (sgsOrder _G, SS.order _G) ] ++
    [let _G = toSn (_C 3 `wr` _S 3) in (sgsOrder _G, SS.order _G) ] ++
    [let _G = toSn (_S 3 `wr` _C 3) in (sgsOrder _G, SS.order _G) ]
    where sgsOrder = orderTGS . tgsFromSgs . SS.sgs

rubikTest = orderSGS (RSS.sgs rubikCube) == 43252003274489856000

ssTest = all (uncurry (==)) ssTests

ssTests =
    [(L.sort $ P.elts $ _C n, L.sort $ SS.elts $ _C n) | n <- [2..6] ]
    ++ [(L.sort $ P.elts $ _D2 n, L.sort $ SS.elts $ _D2 n) | n <- [3..6] ]
    ++ [(L.sort $ P.elts $ _S n, L.sort $ SS.elts $ _S n) | n <- [3..5] ]
    ++ [(L.sort $ P.elts $ _A n, L.sort $ SS.elts $ _A n) | n <- [3..5] ]
    ++ [let _G = toSn (_S 3 `dp` _S 3) in (L.sort $ P.elts _G, L.sort $ SS.elts _G) ]
    ++ [let _G = toSn (_C 3 `wr` _S 3) in (L.sort $ P.elts _G, L.sort $ SS.elts _G) ]
    ++ [let _G = toSn (_S 3 `wr` _C 3) in (L.sort $ P.elts _G, L.sort $ SS.elts _G) ]

ccTest = and ccTests

ccTests =
    [conjClassReps (graphAuts (c 5)) == [(p [],1),(p [[1,2],[3,5]],5),(p [[1,2,3,4,5]],2),(p [[1,3,5,2,4]],2)]
    ,conjClassReps (graphAuts (q 3)) ==
        [(p [],1)
        ,(p [[0,1],[2,3],[4,5],[6,7]],3)
        ,(p [[0,1],[2,5],[3,4],[6,7]],6)
        ,(p [[0,1,3,2],[4,5,7,6]],6)
        ,(p [[0,1,3,7,6,4],[2,5]],8)
        ,(p [[0,3],[1,2],[4,7],[5,6]],3)
        ,(p [[0,3,6,5],[1,2,7,4]],6)
        ,(p [[0,3,6],[1,7,4]],8)
        ,(p [[0,3],[4,7]],6)
        ,(p [[0,7],[1,6],[2,5],[3,4]],1)
        ]
    ]