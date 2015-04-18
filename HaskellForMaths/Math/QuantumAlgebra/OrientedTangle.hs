-- Copyright (c) David Amos, 2010. All rights reserved.

{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}


module Math.QuantumAlgebra.OrientedTangle where

import Math.Algebra.Field.Base
import Math.Algebras.LaurentPoly -- hiding (lvar, q, q')

import Math.QuantumAlgebra.TensorCategory

import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures

-- import MathExperiments.Algebra.TAlgebra


-- ORIENTED TANGLE CATEGORY

data Oriented = Plus | Minus deriving (Eq,Ord,Show)

data HorizDir = ToL | ToR deriving (Eq,Ord,Show)

data OrientedTangle

-- In GHCi 6.12.1, we appear to be limited to 8 value constructors within an associated data family
instance Category OrientedTangle where
    data Ob OrientedTangle = OT [Oriented] deriving (Eq,Ord,Show)
    data Ar OrientedTangle = IdT [Oriented]
                           | CapT HorizDir
                           | CupT HorizDir
                           | XPlus | XMinus
                           | SeqT [Ar OrientedTangle]
                           | ParT [Ar OrientedTangle]
                           deriving (Eq,Ord,Show)
    id_ (OT os) = IdT os
    source (IdT os) = OT os
    source (CapT _) = OT []
    source (CupT toR) = OT [Plus,Minus]
    source (CupT toL) = OT [Minus,Plus]
    source XPlus = OT [Plus,Plus]
    source XMinus = OT [Plus,Plus]
    source (ParT as) = OT $ concatMap ((\(OT os) -> os) . source) as
    source (SeqT as) = source (head as)
    target (IdT os) = OT os
    target (CapT toR) = OT [Minus,Plus]
    target (CapT toL) = OT [Plus,Minus]
    target (CupT _) = OT []
    target XPlus = OT [Plus,Plus]
    target XMinus = OT [Plus,Plus]
    target (ParT as) = OT $ concatMap ((\(OT os) -> os) . target) as
    target (SeqT as) = target (last as)
    a >>> b | target a == source b = SeqT [a,b]

instance TensorCategory OrientedTangle where
    tunit = OT []
    tob (OT as) (OT bs) = OT (as++bs)
    tar a b = ParT [a,b]



idV = id
idV' = id

evalV  = \(E i, E j) -> if i + j == 0 then return () else zero
evalV' = \(E i, E j) -> if i + j == 0 then return () else zero

coevalV  m = foldl (<+>) zero [e i `te` e (-i) | i <- [1..m] ]
coevalV' m = foldl (<+>) zero [e (-i) `te` e i | i <- [1..m] ]

lambda m = q' ^ m -- q^-m

c m (E i, E j) = case compare i j of
                      EQ -> (lambda m * q) *> return (E i, E i)
                      LT -> lambda m *> return (E j, E i)
                      GT -> lambda m *> (return (E j, E i) <+> (q - q') *> return (E i, E j))

-- inverse of c
c' m (E i, E j) = case compare i j of
                       EQ -> (1/(lambda m * q)) *> return (E i, E i)
                       LT -> (1/lambda m) *> (return (E j, E i) <+> (q'-q) *> return (E i, E j))
                       GT -> (1/lambda m) *> return (E j, E i)

testcc' m v = nf $ v >>= c m >>= c' m

mu m (E i) = (1 / (lambda m * q ^ (2*i-1))) *> return (E i)

mu' m (E i) = (lambda m * q ^ (2*i-1)) *> return (E i)

-- The following are modified from Kassel. We compose diagrams downwards, whereas he composes them upwards.

capRL m = coevalV m

capLR m = do
    (i,j) <- coevalV' m
    k <- mu' m j
    return (i,k)

cupRL m = evalV

cupLR m (i,j) = do
    k <- mu m i
    evalV' (k,j)    
-- linear evalV' . (linear (mu' m) `tf` idV)



xplus m = c m

xminus m = c' m

yplus m (p,q) = do
    (r,s) <- capRL m
    (t,u) <- xplus m (q,r)
    cupRL m (p,t)
    return (u,s)

yminus m (p,q) = do
    (r,s) <- capRL m
    (t,u) <- xminus m (q,r)
    cupRL m (p,t)
    return (u,s)

tplus m (p,q) = do
    (r,s) <- capLR m
    (t,u) <- xplus m (s,p)
    cupLR m (u,q)
    return (r,t)

tminus m (p,q) = do
    (r,s) <- capLR m
    (t,u) <- xminus m (s,p)
    cupLR m (u,q)
    return (r,t)

zplus m (p,q) = do
    (r,u) <- capLR m
    (s,t) <- capLR m
    (v,w) <- xplus m (t,u)
    cupLR m (v,q)
    cupLR m (w,p)
    return (r,s)

zminus m (p,q) = do
    (r,u) <- capLR m
    (s,t) <- capLR m
    (v,w) <- xminus m (t,u)
    cupLR m (v,q)
    cupLR m (w,p)
    return (r,s)

{-
Then we have for example the following:
> let v = e1 `te` e2 in nf $ v >>= xplus 2 >>= xminus 2
(e1,e2)
> let v = e (-1) `te` e2 in nf $ v >>= yplus 2 >>= tminus 2
(e-1,e2)
> let v = e (-1) `te` e (-2) in nf $ v >>= zplus 2 >>= zminus 2
(e-1,e-2)
-}


oloop m = nf $ do
    (a,b) <- capLR m
    cupRL m (a,b)

-- oriented trefoil
otrefoil m = nf $ do
    (p,q) <- capLR m
    (r,s) <- capLR m
    (t,u) <- tminus m (q,r)
    (v,w) <- zminus m (p,t)
    (x,y) <- xminus m (u,s)
    cupRL m (w,x)
    cupRL m (v,y)

-- oriented the other way
otrefoil' m = nf $ do
    (p,q) <- capRL m
    (r,s) <- capRL m
    (t,u) <- yminus m (q,r)
    (v,w) <- xminus m (p,t)
    (x,y) <- zminus m (u,s)
    cupLR m (w,x)
    cupLR m (v,y)


{-
-- REPRESENTATIONS OF THE TANGLE CATEGORY IN VECTOR SPACE CATEGORY
-- But we need to convert the above code to use TensorAlgebra first

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
-}