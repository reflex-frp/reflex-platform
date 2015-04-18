-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

-- |A module defining classes and example instances of categories and tensor categories
module Math.QuantumAlgebra.TensorCategory where

import Math.Algebra.Group.PermutationGroup


class Category c where
    data Ob c :: *
    data Ar c :: *
    id_ :: Ob c -> Ar c
    source, target :: Ar c -> Ob c
    (>>>) :: Ar c -> Ar c -> Ar c
-- Note that the class Category defined in Control.Category is about categories whose objects are Haskell types,
-- whereas we want the objects to be values of a single type.


-- Kassel p282
-- The following is actually definition of a strict tensor category
class Category c => TensorCategory c where
    tunit :: Ob c
    tob :: Ob c -> Ob c -> Ob c -- tensor product of objects
    tar :: Ar c -> Ar c -> Ar c -- tensor product of arrows

class TensorCategory c => StrictTensorCategory c where {}
-- we want to be able to declare some tensor categories as strict

class TensorCategory c => WeakTensorCategory c where
    assoc :: Ob c -> Ob c -> Ob c -- (u `tob` v) `tob` w -> u `tob` (v `tob` w)
    lunit :: Ob c -> Ob c         -- unit `tob` v -> v
    runit :: Ob c -> Ob c         -- v `tob` unit -> v

{-
instance (TensorCategory c, Eq (Ar c), Show (Ar c)) => Num (Ar c) where
    (*) = tar
-}


-- SYMMETRIC GROUPOID

data SymmetricGroupoid

instance Category SymmetricGroupoid where
    data Ob SymmetricGroupoid = OS Int deriving (Eq,Ord,Show)
    data Ar SymmetricGroupoid = AS Int (Permutation Int) deriving (Eq,Ord,Show)
    id_ (OS n) = AS n 1
    source (AS n _) = OS n
    target (AS n _) = OS n
    AS m g >>> AS n h | m == n = AS m (g*h)

instance TensorCategory SymmetricGroupoid where
    tunit = OS 0
    tob (OS m) (OS n) = OS (m+n)
    tar (AS m g) (AS n h) = AS (m+n) (g * h~^k)
        where k = p [[1..m+n]] ^ m
--    tar (AS m g) (AS n h) = AS (m+n) (fromPairs $ toPairs g ++ map (\(x,y)->(x+m,y+m)) (toPairs h))


-- BRAID CATEGORY

data Braid

instance Category Braid where
    data Ob Braid = OB Int deriving (Eq,Ord,Show)
    data Ar Braid = AB Int [Int] deriving (Eq,Ord,Show)
    id_ (OB n) = AB n []
    source (AB n _) = OB n
    target (AB n _) = OB n
    AB m is >>> AB n js | m == n = AB m (is ++ js)

s n i | 0 < i && i < n = AB n [i]

instance TensorCategory Braid where
    tunit = OB 0
    tob (OB a) (OB b) = OB (a+b)
    tar (AB m is) (AB n js) = AB (m+n) (is ++ map (+m) js)




data Cob2
-- works very similar to Tangle category

instance Category Cob2 where
    data Ob Cob2 = O Int deriving (Eq,Ord,Show)
    data Ar Cob2 = Id Int
                 | Unit
                 | Mult
                 | Counit
                 | Comult
                 | Par (Ar Cob2) (Ar Cob2)
                 | Seq (Ar Cob2) (Ar Cob2)
                 deriving (Eq,Ord,Show)
    id_ (O n) = Id n
    source (Id n) = O n
    source Unit = O 0
    source Mult = O 2
    source Counit = O 1
    source Comult = O 1
    source (Par a b) = O (sa + sb) where O sa = source a; O sb = source b
    source (Seq a b) = source a
    target (Id n) = O n
    target Unit = O 1
    target Mult = O 1
    target Counit = O 0
    target Comult = O 2
    target (Par a b) = O (ta + tb) where O ta = target a; O tb = target b
    target (Seq a b) = target b
    a >>> b | target a == source b = Seq a b

instance TensorCategory Cob2 where
    tunit = O 0
    tob (O a) (O b) = O (a+b)
    tar a b = Par a b

-- rewrite a Cob2 so that it is a Seq of Pars
-- (this isn't necessarily going to help us towards a normal form - there may not even be one
rewrite (Par (Seq a1 a2) (Seq b1 b2)) =
    Seq (Par idSourceA b1')
        ( (Seq (Par idSourceA b2')
               (Seq (Par a1' idTargetB)
                    (Par a2' idTargetB) ) ) )
    where idSourceA = id_ (source a1)
          idTargetB = id_ (target b2)
          a1' = rewrite a1
          a2' = rewrite a2
          b1' = rewrite b1
          b2' = rewrite b2
rewrite x = x