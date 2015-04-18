module Math.Test.TFiniteGeometry where

import Math.Combinatorics.FiniteGeometry
import Math.Core.Field
-- import Math.Algebra.Field.Base
-- import Math.Algebra.Field.Extension
import Math.Combinatorics.GraphAuts
import Math.Algebra.Group.PermutationGroup

test = and
    [numFlatsAG 2 2 0 == length (flatsAG 2 f2 0)
    ,numFlatsAG 2 2 1 == length (flatsAG 2 f2 1)
    ,numFlatsAG 2 2 2 == length (flatsAG 2 f2 2)
    ,numFlatsAG 3 2 1 == length (flatsAG 3 f2 1)
    ,numFlatsAG 3 3 1 == length (flatsAG 3 f3 1)
    ,numFlatsAG 3 4 1 == length (flatsAG 3 f4 1)
    ,numFlatsAG 3 4 2 == length (flatsAG 3 f4 2)
    ,numFlatsAG 3 4 3 == length (flatsAG 3 f4 3)
    ,numFlatsPG 2 2 0 == length (flatsPG 2 f2 0)
    ,numFlatsPG 2 2 1 == length (flatsPG 2 f2 1)
    ,numFlatsPG 2 2 2 == length (flatsPG 2 f2 2)
    ,numFlatsPG 3 2 1 == length (flatsPG 3 f2 1)
    ,numFlatsPG 3 3 1 == length (flatsPG 3 f3 1)
    ,numFlatsPG 3 4 1 == length (flatsPG 3 f4 1)
    ,numFlatsPG 3 4 2 == length (flatsPG 3 f4 2)
    ,numFlatsPG 3 4 3 == length (flatsPG 3 f4 3)
    ,(orderSGS $ incidenceAuts $ incidenceGraphAG 2 f2) == orderAff 2 2 -- * toInteger (degree f2) 
    ,(orderSGS $ incidenceAuts $ incidenceGraphAG 2 f3) == orderAff 2 3 -- * toInteger (degree f3)
    ,(orderSGS $ incidenceAuts $ incidenceGraphAG 2 f4) == orderAff 2 4 * 2 -- * toInteger (degree f4)
    ,(orderSGS $ incidenceAuts $ incidenceGraphPG 2 f2) == orderPGL 3 2 -- * toInteger (degree f2)
    ,(orderSGS $ incidenceAuts $ incidenceGraphPG 2 f3) == orderPGL 3 3 -- * toInteger (degree f3)
    ,(orderSGS $ incidenceAuts $ incidenceGraphPG 2 f4) == orderPGL 3 4 * 2 -- * toInteger (degree f4)
    ]
