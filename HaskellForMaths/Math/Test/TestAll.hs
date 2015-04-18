module Math.Test.TestAll where

import Math.Test.TGraph
import Math.Test.TDesign
import Math.Test.TPermutationGroup
import Math.Test.TSubquotients
import Math.Test.TFiniteGeometry
import Math.Test.TNonCommutativeAlgebra
import Math.Test.TField
import Math.Test.TRootSystem

import Math.Test.TCore.TField
import Math.Test.TCore.TUtils

import Math.Test.TAlgebras.TGroupAlgebra
import Math.Test.TAlgebras.TOctonions
import Math.Test.TAlgebras.TTensorAlgebra
import Math.Test.TAlgebras.TTensorProduct
import Math.Test.TCombinatorics.TCombinatorialHopfAlgebra
import Math.Test.TCombinatorics.TDigraph
import Math.Test.TCombinatorics.TFiniteGeometry
import Math.Test.TCombinatorics.TGraphAuts
import Math.Test.TCombinatorics.TIncidenceAlgebra
import Math.Test.TCombinatorics.TMatroid
import Math.Test.TCombinatorics.TPoset
import Math.Test.TCommutativeAlgebra.TPolynomial
import Math.Test.TCommutativeAlgebra.TGroebnerBasis
import Math.Test.TNumberTheory.TPrimeFactor
import Math.Test.TNumberTheory.TQuadraticField
import Math.Test.TProjects.TMiniquaternionGeometry


import Test.QuickCheck
import Test.HUnit

-- legacy tests - should really be converted to HUnit
testall = and
    [Math.Test.TGraph.test
    ,Math.Test.TDesign.test
    ,Math.Test.TPermutationGroup.test
    ,Math.Test.TSubquotients.test
    ,Math.Test.TFiniteGeometry.test
    ,Math.Test.TField.test
    ,Math.Test.TRootSystem.test
    ]

quickCheckAll =
    do
    -- quickCheck prop_NonCommRingNPoly
    quickCheckUtils
    quickCheck prop_GroupPerm
    quickCheckField
    quickCheckTensorProduct
    quickCheckGroupAlgebra
    quickCheckTensorAlgebra
    putStrLn "Testing Octonions..."
    quickCheck prop_AlgebraNonAssociative_Octonions
    quickCheck prop_InverseLoop_Octonions
    putStrLn "Testing miniquaternion geometries..."
    quickCheck prop_NearFieldF9
    quickCheck prop_NearFieldJ9
    quickCheckCombinatorialHopfAlgebra

hunitAll = runTestTT $ TestList [
    testlistGroupAlgebra,
    testlistCHA,
    testlistDigraph,
    testlistFiniteGeometry,
    testlistGraphAuts,
    testlistIncidenceAlgebra,
    testlistMatroid,
    testlistPoset,
    testlistPolynomial,
    testlistGroebnerBasis,
    testlistPrimeFactor,
    testlistQuadraticField
    ]
