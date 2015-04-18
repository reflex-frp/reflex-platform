-- Copyright (c) 2012, David Amos. All rights reserved.

{-# LANGUAGE FlexibleInstances #-}

module Math.Test.TCombinatorics.TCombinatorialHopfAlgebra where

import Data.List as L

import Math.Core.Field
import Math.Combinatorics.Poset (integerPartitions)

import Math.Algebras.VectorSpace hiding (E)
import Math.Algebras.TensorProduct -- for ghci
import Math.Algebras.Structures

import Math.Combinatorics.CombinatorialHopfAlgebra

import Math.Test.TAlgebras.TVectorSpace hiding (T, f)
import Math.Test.TAlgebras.TTensorProduct
import Math.Test.TAlgebras.TStructures

import Test.QuickCheck
import Test.HUnit

quickCheckCombinatorialHopfAlgebra = do
    quickCheckShuffleAlgebra
    quickCheckSSymF
    quickCheckSSymM
    quickCheckYSymF
    quickCheckYSymM
    quickCheckQSymM
    quickCheckQSymF
    quickCheckSymM
    quickCheckSymE
    quickCheckSymH
    quickCheckNSym
    quickCheckCHAIsomorphism
    quickCheckCHAMorphism


instance Arbitrary a => Arbitrary (Shuffle a) where
    arbitrary = fmap (Sh . take 3) arbitrary

quickCheckShuffleAlgebra = do
    putStrLn "Checking shuffle algebra"
    -- quickCheck (prop_Algebra :: (Q, Vect Q (Shuffle Int), Vect Q (Shuffle Int), Vect Q (Shuffle Int)) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q (Shuffle Int) -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q (Shuffle Int), Vect Q (Shuffle Int)) -> Bool) -- slow
    quickCheck (prop_HopfAlgebra :: Vect Q (Shuffle Int) -> Bool)
    quickCheck (prop_Commutative :: (Vect Q (Shuffle Int), Vect Q (Shuffle Int)) -> Bool)


instance Arbitrary SSymF where
    arbitrary = do xs <- elements permsTo3
                   return (SSymF xs)
        where permsTo3 = concatMap (\n -> L.permutations [1..n]) [0..3]

instance Arbitrary SSymM where
    arbitrary = do xs <- elements permsTo3
                   return (SSymM xs)
        where permsTo3 = concatMap (\n -> L.permutations [1..n]) [0..3]

quickCheckSSymF = do
    putStrLn "Checking SSymF"
    -- quickCheck (prop_Algebra :: (Q, Vect Q SSymF, Vect Q SSymF, Vect Q SSymF) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q SSymF -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q SSymF, Vect Q SSymF) -> Bool)
    quickCheck (prop_HopfAlgebra :: Vect Q SSymF -> Bool)

quickCheckSSymM = do
    putStrLn "Checking SSymM"
    -- quickCheck (prop_Algebra :: (Q, Vect Q SSymM, Vect Q SSymM, Vect Q SSymM) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q SSymM -> Bool)
    -- quickCheck (prop_Bialgebra :: (Q, Vect Q SSymM, Vect Q SSymM) -> Bool) -- too slow
    quickCheck (prop_HopfAlgebra :: Vect Q SSymM -> Bool)

quickCheckDualSSymF = do
    putStrLn "Checking Dual(SSymF)"
    -- quickCheck (prop_Algebra :: (Q, Vect Q (Dual SSymF), Vect Q (Dual SSymF), Vect Q (Dual SSymF)) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q (Dual SSymF) -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q (Dual SSymF), Vect Q (Dual SSymF)) -> Bool)
    quickCheck (prop_HopfAlgebra :: Vect Q (Dual SSymF) -> Bool)

instance Arbitrary (YSymF ()) where
    arbitrary = fmap YSymF (elements (concatMap trees [0..3]))
    -- arbitrary = fmap (YSymF . shape . descendingTree . take 3) (arbitrary :: Gen [Int])
-- We use descendingTree because it can make trees of interesting shapes from a given list
-- but we could equally have used other tree construction methods such as binary search tree

instance Arbitrary (YSymF Int) where
    arbitrary = fmap (YSymF . descendingTree . take 3) (arbitrary :: Gen [Int])
-- It seems to all work even if we leave the labels on. Perhaps we should really put random labels on though,
-- rather than leaving the descendingTree labels

instance Arbitrary (YSymM) where
    arbitrary = fmap YSymM (elements (concatMap trees [0..3]))
    -- arbitrary = fmap (YSymM . shape . descendingTree . take 3) (arbitrary :: Gen [Int])

quickCheckYSymF = do
    putStrLn "Checking YSymF"
    -- quickCheck (prop_Algebra :: (Q, Vect Q (YSymF ()), Vect Q (YSymF ()), Vect Q (YSymF ())) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q (YSymF ()) -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q (YSymF ()), Vect Q (YSymF ())) -> Bool)
    quickCheck (prop_HopfAlgebra :: Vect Q (YSymF ()) -> Bool)

quickCheckYSymM = do
    putStrLn "Checking YSymM"
    -- quickCheck (prop_Algebra :: (Q, Vect Q YSymM, Vect Q YSymM, Vect Q YSymM) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q YSymM -> Bool)
    -- quickCheck (prop_Bialgebra :: (Q, Vect Q YSymM, Vect Q YSymM) -> Bool)
    quickCheck (prop_HopfAlgebra :: Vect Q YSymM -> Bool)


instance Arbitrary QSymM where
    arbitrary = do xs <- elements compositionsTo3
                   return (QSymM xs)
        where compositionsTo3 = concatMap compositions [0..3]

instance Arbitrary QSymF where
    arbitrary = do xs <- elements compositionsTo3
                   return (QSymF xs)
        where compositionsTo3 = concatMap compositions [0..3]

quickCheckQSymM = do
    putStrLn "Checking QSymM"
    quickCheck (prop_Algebra :: (Q, Vect Q QSymM, Vect Q QSymM, Vect Q QSymM) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q QSymM -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q QSymM, Vect Q QSymM) -> Bool)
    quickCheck (prop_HopfAlgebra :: (Vect Q QSymM) -> Bool)
    quickCheck (prop_Commutative :: (Vect Q QSymM, Vect Q QSymM) -> Bool)

quickCheckQSymF = do
    putStrLn "Checking QSymF"
    quickCheck (prop_Algebra :: (Q, Vect Q QSymF, Vect Q QSymF, Vect Q QSymF) -> Bool) -- too slow
    quickCheck (prop_Coalgebra :: Vect Q QSymF -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q QSymF, Vect Q QSymF) -> Bool)
    quickCheck (prop_HopfAlgebra :: (Vect Q QSymF) -> Bool)
    quickCheck (prop_Commutative :: (Vect Q QSymF, Vect Q QSymF) -> Bool)

instance Arbitrary SymM where
    arbitrary = do xs <- elements (concatMap integerPartitions [0..4])
                   return (SymM xs)

quickCheckSymM = do
    putStrLn "Checking SymM"
    quickCheck (prop_Algebra :: (Q, Vect Q SymM, Vect Q SymM, Vect Q SymM) -> Bool)
    quickCheck (prop_Coalgebra :: Vect Q SymM -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q SymM, Vect Q SymM) -> Bool)
    quickCheck (prop_HopfAlgebra :: Vect Q SymM -> Bool)
    quickCheck (prop_Commutative :: (Vect Q SymM, Vect Q SymM) -> Bool)
    quickCheck (prop_Cocommutative :: Vect Q SymM -> Bool)

instance Arbitrary SymE where
    arbitrary = do xs <- elements (concatMap integerPartitions [0..4])
                   return (SymE xs)

quickCheckSymE = do
    putStrLn "Checking SymE"
    quickCheck (prop_Algebra :: (Q, Vect Q SymE, Vect Q SymE, Vect Q SymE) -> Bool)
    quickCheck (prop_Coalgebra :: Vect Q SymE -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q SymE, Vect Q SymE) -> Bool)
    -- quickCheck (prop_HopfAlgebra :: Vect Q SymE -> Bool)
    quickCheck (prop_Commutative :: (Vect Q SymE, Vect Q SymE) -> Bool)
    quickCheck (prop_Cocommutative :: Vect Q SymE -> Bool)

instance Arbitrary SymH where
    arbitrary = do xs <- elements (concatMap integerPartitions [0..4])
                   return (SymH xs)

quickCheckSymH = do
    putStrLn "Checking SymH"
    quickCheck (prop_Algebra :: (Q, Vect Q SymH, Vect Q SymH, Vect Q SymH) -> Bool)
    quickCheck (prop_Coalgebra :: Vect Q SymH -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q SymH, Vect Q SymH) -> Bool)
    -- quickCheck (prop_HopfAlgebra :: (Vect Q SymH) -> Bool)
    quickCheck (prop_Commutative :: (Vect Q SymH, Vect Q SymH) -> Bool)
    quickCheck (prop_Cocommutative :: Vect Q SymH -> Bool)

-- The basis isn't indexed by compositions, but using compositions is an easy way to ensure
-- that we have positive ints and that they're bounded (to keep the comult manageable)
instance Arbitrary NSym where
    arbitrary = do xs <- elements compositionsTo4
                   return (NSym xs)
        where compositionsTo4 = concatMap compositions [0..4]

quickCheckNSym = do
    putStrLn "Checking NSym"
    quickCheck (prop_Algebra :: (Q, Vect Q NSym, Vect Q NSym, Vect Q NSym) -> Bool)
    quickCheck (prop_Coalgebra :: Vect Q NSym -> Bool)
    quickCheck (prop_Bialgebra :: (Q, Vect Q NSym, Vect Q NSym) -> Bool)
    quickCheck (prop_HopfAlgebra :: Vect Q NSym -> Bool)


quickCheckCHAIsomorphism = do
    putStrLn "Checking CHA isomorphism (change of basis)"
    putStrLn "Checking bijections"
    quickCheck (prop_Id (ssymMtoF . ssymFtoM) :: Vect Q SSymF -> Bool)
    quickCheck (prop_Id (ssymFtoM . ssymMtoF) :: Vect Q SSymM -> Bool)
    quickCheck (prop_Id (ysymMtoF . ysymFtoM) :: Vect Q (YSymF ()) -> Bool)
    quickCheck (prop_Id (ysymFtoM . ysymMtoF) :: Vect Q YSymM -> Bool)
    quickCheck (prop_Id (qsymMtoF . qsymFtoM) :: Vect Q QSymF -> Bool)
    quickCheck (prop_Id (qsymFtoM . qsymMtoF) :: Vect Q QSymM -> Bool)
    putStrLn "Checking morphisms"
    putStrLn "SSym"
    -- quickCheck (prop_AlgebraMorphism ssymMtoF :: (Q, Vect Q SSymM, Vect Q SSymM) -> Bool) -- too slow
    -- quickCheck (prop_AlgebraMorphism ssymFtoM :: (Q, Vect Q SSymF, Vect Q SSymF) -> Bool) -- too slow
    quickCheck (prop_CoalgebraMorphism ssymMtoF :: Vect Q SSymM -> Bool)
    quickCheck (prop_CoalgebraMorphism ssymFtoM :: Vect Q SSymF -> Bool)
    quickCheck (prop_HopfAlgebraMorphism ssymFtoM :: Vect Q SSymF -> Bool)
    quickCheck (prop_HopfAlgebraMorphism ssymMtoF :: Vect Q SSymM -> Bool)
    quickCheck (prop_AlgebraMorphism ssymFtoDual :: (Q, Vect Q SSymF, Vect Q SSymF) -> Bool)
    quickCheck (prop_CoalgebraMorphism ssymFtoDual :: Vect Q SSymF -> Bool)
    quickCheck (prop_HopfAlgebraMorphism ssymFtoDual :: Vect Q SSymF -> Bool)
    putStrLn "YSym"
    -- quickCheck (prop_AlgebraMorphism ysymMtoF :: (Q, Vect Q YSymM, Vect Q YSymM) -> Bool) -- too slow
    quickCheck (prop_AlgebraMorphism ysymFtoM :: (Q, Vect Q (YSymF ()), Vect Q (YSymF ())) -> Bool)
    quickCheck (prop_CoalgebraMorphism ysymMtoF :: Vect Q YSymM -> Bool)
    quickCheck (prop_CoalgebraMorphism ysymFtoM :: Vect Q (YSymF ()) -> Bool)
    quickCheck (prop_HopfAlgebraMorphism ysymMtoF :: Vect Q YSymM -> Bool)
    quickCheck (prop_HopfAlgebraMorphism ysymFtoM :: Vect Q (YSymF ()) -> Bool)
    putStrLn "QSym"
    quickCheck (prop_AlgebraMorphism qsymMtoF :: (Q, Vect Q QSymM, Vect Q QSymM) -> Bool)
    quickCheck (prop_AlgebraMorphism qsymFtoM :: (Q, Vect Q QSymF, Vect Q QSymF) -> Bool)
    quickCheck (prop_CoalgebraMorphism qsymMtoF :: Vect Q QSymM -> Bool)
    quickCheck (prop_CoalgebraMorphism qsymFtoM :: Vect Q QSymF -> Bool)
    quickCheck (prop_HopfAlgebraMorphism qsymFtoM :: Vect Q QSymF -> Bool)
    quickCheck (prop_HopfAlgebraMorphism qsymMtoF :: Vect Q QSymM -> Bool)
    putStrLn "Sym"
    quickCheck (prop_AlgebraMorphism symEtoM :: (Q, Vect Q SymE, Vect Q SymE) -> Bool)
    quickCheck (prop_AlgebraMorphism symHtoM :: (Q, Vect Q SymH, Vect Q SymH) -> Bool)
    quickCheck (prop_CoalgebraMorphism symEtoM :: Vect Q SymE -> Bool)
    quickCheck (prop_CoalgebraMorphism symHtoM :: Vect Q SymH -> Bool)
    where prop_Id f x = f x == x 

quickCheckCHAMorphism = do
    putStrLn "Checking morphisms between CHAs"
    quickCheck (prop_AlgebraMorphism descendingTreeMap :: (Q, Vect Q SSymF, Vect Q SSymF) -> Bool)
    quickCheck (prop_CoalgebraMorphism descendingTreeMap :: Vect Q SSymF -> Bool)
    quickCheck (prop_HopfAlgebraMorphism descendingTreeMap :: Vect Q SSymF -> Bool)
    quickCheck (prop_AlgebraMorphism descentMap :: (Q, Vect Q SSymF, Vect Q SSymF) -> Bool)
    quickCheck (prop_CoalgebraMorphism descentMap :: Vect Q SSymF -> Bool)
    quickCheck (prop_HopfAlgebraMorphism descentMap :: Vect Q SSymF -> Bool)
    quickCheck (prop_AlgebraMorphism leftLeafCompositionMap :: (Q, Vect Q (YSymF ()), Vect Q (YSymF ())) -> Bool)
    quickCheck (prop_CoalgebraMorphism leftLeafCompositionMap :: Vect Q (YSymF ()) -> Bool)
    quickCheck (prop_HopfAlgebraMorphism leftLeafCompositionMap :: Vect Q (YSymF ()) -> Bool)
    quickCheck (\x -> descentMap x == (leftLeafCompositionMap . descendingTreeMap) (x :: Vect Q SSymF))
    quickCheck (prop_AlgebraMorphism symToQSymM :: (Q, Vect Q SymM, Vect Q SymM) -> Bool)
    quickCheck (prop_CoalgebraMorphism symToQSymM :: Vect Q SymM -> Bool)
    quickCheck (prop_HopfAlgebraMorphism symToQSymM :: Vect Q SymM -> Bool)
    -- quickCheck (prop_AlgebraMorphism nsymToSSym :: (Q, Vect Q NSym, Vect Q NSym) -> Bool) -- too slow
    quickCheck (prop_CoalgebraMorphism nsymToSSym :: Vect Q NSym -> Bool)
    quickCheck (prop_HopfAlgebraMorphism nsymToSSym :: Vect Q NSym -> Bool)
    quickCheck (prop_AlgebraMorphism nsymToSymH :: (Q, Vect Q NSym, Vect Q NSym) -> Bool)
    quickCheck (prop_CoalgebraMorphism nsymToSymH :: Vect Q NSym -> Bool)
    -- The map NSym -> Sym factors through the descent map SSym -> (YSym ->) QSym
    quickCheck (\x -> (symToQSymM . symHtoM . nsymToSymH) x == (qsymFtoM . descentMap . nsymToSSym) (x :: Vect Q NSym))
    -- Coalgebra morphisms showing that various Hopf algebras are cofree
    quickCheck (prop_CoalgebraMorphism ysymmToSh :: Vect Q YSymM -> Bool)
    -- Duality pairings
    quickCheck (prop_HopfPairing :: (Vect Q SSymF, Vect Q SSymF, Vect Q (Dual SSymF), Vect Q (Dual SSymF)) -> Bool)
    quickCheck (prop_HopfPairing :: (Vect Q SSymF, Vect Q SSymF, Vect Q SSymF, Vect Q SSymF) -> Bool)
    quickCheck (prop_BialgebraPairing :: (Vect Q SymH, Vect Q SymH, Vect Q SymM, Vect Q SymM) -> Bool)
    -- The above is in fact a Hopf pairing, but need to define a Hopf algebra instance for SymH
    quickCheck (prop_HopfPairing :: (Vect Q NSym, Vect Q NSym, Vect Q QSymM, Vect Q QSymM) -> Bool)
    -- A bialgebra pairing <A,B> gives a map A -> B*, u -> <u,.>
    -- However, require that the pairing is non-degenerate in order to be injective, and also need to prove surjective


testlistCHA = TestList [
    TestCase $ assertEqual "ysymMtoF" (ysymMtoF $ ysymM $ T (T E () E) () (T (T E () E) () E))
               ( ysymF (T (T E () E) () (T (T E () E) () E)) - ysymF (T (T E () E) () (T E () (T E () E)))
               - ysymF (T E () (T E () (T (T E () E) () E))) + ysymF (T E () (T E () (T E () (T E () E)))) ), -- Loday.pdf, p10
    TestCase $ assertEqual "leftLeafComposition" [2,3,2,1]
               (leftLeafComposition $ T (T (T E 1 E) 2 (T (T E 3 E) 4 E)) 5 (T (T E 6 E) 7 (T E 8 E))), -- Loday.pdf, p6
    TestCase $ assertEqual "mult QSymM" (qsymM [1,3] + qsymM [3,1] + qsymM [1,1,2] + qsymM [1,2,1] + qsymM [2,1,1])
                                        (qsymM [2] * qsymM [1,1]), -- SSym.pdf, p5
    TestCase $ assertEqual "mult QSymM" (qsymM [1,3] + qsymM [2,2] + 2*qsymM [1,1,2] + qsymM [1,2,1])
                                        (qsymM [1] * qsymM [1,2]), -- SSym.pdf, p31
    TestCase $ assertEqual "mult SSymF" (ssymM [1,2,4,3]+ssymM [1,3,4,2]+ssymM [1,4,2,3]+3*ssymM [1,4,3,2]+ssymM [2,3,4,1]+2*ssymM [2,4,3,1]
                                        +ssymM [3,4,2,1]+ssymM [4,1,2,3]+2*ssymM [4,1,3,2]+ssymM [4,2,3,1]+ssymM [4,3,1,2])
                                        (ssymM [1,2] * ssymM [2,1]), -- SSym.pdf, p15
    TestCase $ assertEqual "ssymMtoF" (ssymF [4,1,2,3] - ssymF [4,1,3,2] - ssymF [4,2,1,3] + ssymF [4,3,2,1])
                                     (ssymMtoF (ssymM [4,1,2,3])), -- SSym.pdf, p7
    TestCase $ assertEqual "antipode NSym" (- nsym [1,1,1] + nsym [1,2] + nsym [2,1] - nsym [3])
                                           (antipode $ nsym [3]) -- Hazewinkel p142
    ]
