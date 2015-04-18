-- Copyright (c) 2012, David Amos. All rights reserved.

module Math.Test.TCore.TUtils where

import Data.List as L

import Test.QuickCheck

import Math.Core.Utils

quickCheckUtils = do
    putStrLn "Testing Math.Core.Utils"
    quickCheck prop_setUnionAsc
    quickCheck prop_multisetSumAsc
    quickCheck prop_multisetSumDesc
    quickCheck prop_diffAsc
    quickCheck prop_diffDesc

prop_setUnionAsc xs ys = setUnionAsc xs' ys' == zs'
    where xs' = toSet xs :: [Int]
          ys' = toSet ys
          zs' = toSet (xs++ys)

prop_multisetSumAsc xs ys = multisetSumAsc xs' ys' == zs'
    where xs' = L.sort xs :: [Int]
          ys' = L.sort ys
          zs' = L.sort (xs ++ ys)

prop_multisetSumDesc xs ys = multisetSumDesc xs' ys' == zs'
    where xs' = reverse (L.sort xs) :: [Int]
          ys' = reverse (L.sort ys)
          zs' = reverse (L.sort (xs ++ ys))

prop_diffAsc xs ys = diffAsc xs' ys' == xs' \\ ys'
    where xs' = L.sort xs :: [Int]
          ys' = L.sort ys

prop_diffDesc xs ys = diffDesc xs' ys' == xs' \\ ys'
    where xs' = reverse (L.sort xs) :: [Int]
          ys' = reverse (L.sort ys)

-- !! Feels like we need a better negative test
-- xs is never submultiset of symmetric difference xs ys, unless null ys
prop_isSubMultisetAsc xs ys = isSubMultisetAsc xs' zs'
                           && (isSubMultisetAsc zs' xs' `implies` null ys)
                           && (isSubMultisetAsc xs' ys' `implies` (length xs <= length ys)) 
    where xs' = L.sort xs :: [Int]
          ys' = L.sort ys
          zs' = multisetSumAsc xs' ys'
          implies p q = not p || q