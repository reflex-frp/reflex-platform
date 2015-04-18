-- Copyright (c) David Amos, 2008. All rights reserved.

{-# LANGUAGE FlexibleInstances #-}

module Math.Test.TDesign where

import qualified Data.List as L

import Math.Algebra.Field.Base
import Math.Algebra.Field.Extension
import Math.Combinatorics.Design as D
import Math.Algebra.Group.SchreierSims as SS


factorial n = product [1..n]

choose n m | m <= n = product [m+1..n] `div` product [1..n-m]

test = and [designParamsTest, designAutTest]


designParamsTest = and
    [designParams (ag2 f2) == Just (2,(4,2,1))
    ,designParams (ag2 f3) == Just (2,(9,3,1))
    ,designParams (ag2 f4) == Just (2,(16,4,1))
    ,designParams (pg2 f2) == Just (2,(7,3,1))
    ,designParams (pg2 f3) == Just (2,(13,4,1))
    ,designParams (pg2 f4) == Just (2,(21,5,1))
    ,designParams s_3_6_22 == Just (3,(22,6,1))
    ,designParams (derivedDesign s_3_6_22 1) == Just (2,(21,5,1)) -- it is PG(2,F4)
    ,designParams s_4_5_11 == Just (4,(11,5,1))
    ,designParams (derivedDesign (derivedDesign s_4_5_11 0) 1) == Just (2,(9,3,1)) -- it is AG(2,F3)
    ]


designAutTest = all (uncurry (==)) designAutTests

designAutTests =
    [(SS.order $ designAuts $ pg2 f2, 168) -- this is L3(2), see Atlas
    ,(SS.order $ designAuts $ pg2 f3, 5616) -- this is L3(3)
    ,(SS.order $ designAuts $ pg2 f4, 120960) -- this is S3.L3(4)
--    ,(SS.order $ designAuts $ pg2 f5, 372000) -- this is L3(5)
    ]