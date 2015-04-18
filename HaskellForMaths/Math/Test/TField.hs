module Math.Test.TField where

import Math.Algebra.Field.Base
import Math.Algebra.Field.Extension

test = and
    [ (1/5 :: QSqrt3) * 5 == 1 -- regression test for defect
    , (1/4 :: F25) * 4 == 1
    ]