-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, EmptyDataDecls #-}

-- |A module defining the category of tangles, and representations into the category of vector spaces
-- (specifically, knot invariants).
module Math.QuantumAlgebra.Tangle where

-- import qualified Data.List as L

import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures

import Math.Algebra.Field.Base
import Math.Algebras.LaurentPoly

import Math.QuantumAlgebra.TensorCategory


instance Mon [a] where
    munit = []
    mmult = (++)

-- type TensorAlgebra k a = Vect k [a]

instance (Eq k, Num k, Ord a) => Algebra k [a] where
    unit 0 = zero -- V []
    unit x = V [(munit,x)]
    mult = nf . fmap (\(a,b) -> a `mmult` b)

-- Could make TensorAlgebra k a into an instance of Category, TensorCategory
    

-- TANGLE CATEGORY
-- (Unoriented)

data Tangle

instance Category Tangle where
    data Ob Tangle = OT Int deriving (Eq,Ord,Show)
    data Ar Tangle = IdT Int
                   | CapT
                   | CupT
                   | OverT
                   | UnderT
--                   | SeqT (Ar Tangle) (Ar Tangle)
                   | SeqT [Ar Tangle]
--                   | ParT (Ar Tangle) (Ar Tangle)
                   | ParT [Ar Tangle]
                   deriving (Eq,Ord,Show)
    id_ (OT n) = IdT n
    source (IdT n) = OT n
    source CapT = OT 0
    source CupT = OT 2
    source OverT = OT 2
    source UnderT = OT 2
--    source (ParT a b) = OT (sa + sb) where OT sa = source a; OT sb = source b
    source (ParT as) = OT $ sum [sa | a <- as, let OT sa = source a]
--    source (SeqT a b) = source a
    source (SeqT as) = source (head as)
    target (IdT n) = OT n
    target CapT = OT 2
    target CupT = OT 0
    target OverT = OT 2
    target UnderT = OT 2
--    target (ParT a b) = OT (ta + tb) where OT ta = target a; OT tb = target b
    target (ParT as) = OT $ sum [ta | a <- as, let OT ta = target a]
--    target (SeqT a b) = target b
    target (SeqT as) = target (last as)
--    a >>> b | target a == source b = SeqT a b
    a >>> b | target a == source b = SeqT [a,b]

instance TensorCategory Tangle where
    tunit = OT 0
    tob (OT a) (OT b) = OT (a+b)
--    tar a b = ParT a b
    tar a b = ParT [a,b]



-- KAUFFMAN BRACKET

data Oriented = Plus | Minus deriving (Eq,Ord,Show)

type TangleRep b = Vect (LaurentPoly Q) b


-- adapted from http://blog.sigfpe.com/2008/10/untangling-with-continued-fractions.html
cap :: [Oriented] -> TangleRep [Oriented]
cap [] = return [Plus, Minus] <+> (-q^2) *> return [Minus, Plus]

cup :: [Oriented] -> TangleRep [Oriented]
cup [Plus, Minus] = (-q'^2) *> return []
cup [Minus, Plus] = return []
cup _ = zero

-- also called xminus
over :: [Oriented] -> TangleRep [Oriented]
over [u, v] = q  *> do {[] <- cup [u, v]; cap []}
          <+> q' *> return [u, v]

{-
-- if you expand "over" into terms, you find that it equals the following,
-- which strongly resembles c' below
over' (T i j) = case compare i j of
                EQ -> q' *> return (T i i)                                       -- ++ -> q' ++, -- -> q' -- 
                LT -> q  *> return (T j i)                                       -- +- -> q -+
                GT -> q  *> (return (T j i) <+> (q'^2 - q^2) *> return (T i j))  -- -+ -> q +- + (q'-q^3) -+
-}
-- also called xplus
under :: [Oriented] -> TangleRep [Oriented]
under [u, v] = q' *> do {[] <- cup [u, v]; cap []}
           <+> q  *> return [u, v]

{-
-- if you expand "under" into terms, you find that it equals the following,
-- which strongly resembles c below
under' (T i j) = case compare i j of
                 EQ -> q  *> return (T i i)                                       -- ++ -> q ++, -- -> q -- 
                 LT -> q' *> (return (T j i) <+> (q^2 - q'^2) *> return (T i j))  -- +- -> q' -+ + (q-q^-3) -+
                 GT -> q' *> return (T j i)                                       -- -+ -> q' +-
-}
loop = nf $ do {[i, j] <- cap []; cup [i, j]}

trefoil = nf $ do
    [i, j] <- cap []
    [k, l] <- cap []
    [m, n] <- under [j, k]
    [p, q] <- over [i, m]
    [r, s] <- over [n, l]
    cup [p, s]
    cup [q, r]


-- KAUFFMAN BRACKET AS A REPRESENTATION FROM TANGLE TO VECT

-- But this isn't quite the Kauffman bracket - we still need to divide by (-q^2-q^-2)
kauffman :: Ar Tangle -> TangleRep [Oriented] -> TangleRep [Oriented]
kauffman (IdT n) = id -- could be tf of n ids
kauffman CapT = linear cap
kauffman CupT = linear cup
kauffman OverT = linear over
kauffman UnderT = linear under
kauffman (SeqT fs) = foldl (>>>) id $ map kauffman fs
    where g >>> h = h . g
kauffman (ParT [f]) = kauffman f
kauffman (ParT (f:fs)) = tf m (kauffman f) (kauffman (ParT fs))
    where OT m = source f
          tf m f' fs' = linear (\xs -> let (ls,rs) = splitAt m xs in f' (return ls) * fs' (return rs) )
{-
kauffman (ParT f g) = tf m n (kauffman f) (kauffman g)
    where OT m = source f
          OT n = source g
          tf m n f' g' = linear (\xs -> let (ls,rs) = splitAt m xs in f' (return ls) * g' (return rs) )
-}

-- loopT = SeqT CapT CupT
loopT = SeqT [CapT, CupT]

{-
trefoilT = (ParT CapT CapT) `SeqT` (ParT (IdT 1) (ParT UnderT (IdT 1)))
    `SeqT` (ParT OverT OverT) `SeqT` (ParT (IdT 1) (ParT CupT (IdT 1))) `SeqT` CupT

trefoilT = ParT [CapT, CapT]
    `SeqT` ParT [IdT 1, UnderT, IdT 1]
    `SeqT` ParT [OverT, OverT]
    `SeqT` ParT [IdT 1, CupT, IdT 1]
    `SeqT` CupT
-}
trefoilT = SeqT [
    ParT [CapT, CapT],
    ParT [IdT 1, UnderT, IdT 1],
    ParT [OverT, OverT],
    ParT [IdT 1, CupT, IdT 1],
    CupT]
-- eg kauffman (trefoilT) (return [])
