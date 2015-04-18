-- Copyright (c) David Amos, 2010-2012. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE IncoherentInstances #-}

-- |A module defining various algebraic structures that can be defined on vector spaces
-- - specifically algebra, coalgebra, bialgebra, Hopf algebra, module, comodule
module Math.Algebras.Structures where

import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct


-- MONOID

-- |Monoid
class Mon m where
    munit :: m
    mmult :: m -> m -> m


-- ALGEBRAS, COALGEBRAS, BIALGEBRAS, HOPF ALGEBRAS

-- |Caution: If we declare an instance Algebra k b, then we are saying that the vector space Vect k b is a k-algebra.
-- In other words, we are saying that b is the basis for a k-algebra. So a more accurate name for this class
-- would have been AlgebraBasis.
class Algebra k b where
    unit :: k -> Vect k b
    mult :: Vect k (Tensor b b) -> Vect k b

-- |Sometimes it is more convenient to work with this version of unit.
unit' :: (Eq k, Num k, Algebra k b) => Vect k () -> Vect k b
unit' = unit . unwrap -- where unwrap = counit :: Num k => Trivial k -> k

-- |An instance declaration for Coalgebra k b is saying that the vector space Vect k b is a k-coalgebra.
class Coalgebra k b where
    counit :: Vect k b -> k
    comult :: Vect k b -> Vect k (Tensor b b)

-- |Sometimes it is more convenient to work with this version of counit.
counit' :: (Eq k, Num k, Coalgebra k b) => Vect k b -> Vect k ()
counit' = wrap . counit -- where wrap = unit :: Num k => k -> Trivial k

-- unit' and counit' enable us to form tensors of these functions

-- |A bialgebra is an algebra which is also a coalgebra, subject to the compatibility conditions
-- that counit and comult must be algebra morphisms (or equivalently, that unit and mult must be coalgebra morphisms)
class (Algebra k b, Coalgebra k b) => Bialgebra k b where {}

class Bialgebra k b => HopfAlgebra k b where
    antipode :: Vect k b -> Vect k b


instance (Eq k, Num k, Eq b, Ord b, Show b, Algebra k b) => Num (Vect k b) where
    x+y = x <+> y
    negate x = negatev x
    -- negate (V ts) = V $ map (\(b,x) -> (b, negate x)) ts
    x*y = mult (x `te` y)
    fromInteger n = unit (fromInteger n)
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"


-- This is the Frobenius form, provided some conditions are met
-- pairing = counit . mult

{-
-- A class to be used to declare that a type b should be given the set coalgebra structure
class SetCoalgebra b where {}

instance (Num k, SetCoalgebra b) => Coalgebra k b where
    counit (V ts) = sum [x | (m,x) <- ts] -- trace
    comult = fmap (\m -> T m m) -- diagonal
-}


instance (Eq k, Num k) => Algebra k () where
    unit = wrap
    -- unit 0 = zero -- V []
    -- unit x = V [( (),x)]
    mult = fmap (\((),())->())
    -- mult = linear mult' where mult' ((),()) = return ()
    -- mult (V [( ((),()), x)]) = V [( (),x)]
    -- mult (V []) = zerov

instance (Eq k, Num k) => Coalgebra k () where
    counit = unwrap
    -- counit (V []) = 0
    -- counit (V [( (),x)]) = x
    comult = fmap (\()->((),()))
    -- comult = linear comult' where comult' () = return ((),())
    -- comult (V [( (),x)]) = V [( ((),()), x)]
    -- comult (V []) = zerov


-- Kassel p4
-- |The direct sum of k-algebras can itself be given the structure of a k-algebra.
-- This is the product object in the category of k-algebras.
instance (Eq k, Num k, Ord a, Ord b, Algebra k a, Algebra k b) => Algebra k (DSum a b) where
    unit k = i1 (unit k) <+> i2 (unit k)
    -- unit == (i1 . unit) <<+>> (i2 . unit)
    mult = linear mult'
        where mult' (Left a1, Left a2) = i1 $ mult $ return (a1,a2)
              mult' (Right b1, Right b2) = i2 $ mult $ return (b1,b2)
              mult' _ = zero
-- This is the product algebra, which is the product in the category of algebras
-- 1 = (1,1)
-- (a1,b1) * (a2,b2) = (a1*a2, b1*b2)
-- It's not a coproduct, because i1, i2 aren't algebra morphisms (they violate Unit axiom)

-- |The direct sum of k-coalgebras can itself be given the structure of a k-coalgebra.
-- This is the coproduct object in the category of k-coalgebras.
instance (Eq k, Num k, Ord a, Ord b, Coalgebra k a, Coalgebra k b) => Coalgebra k (DSum a b) where
    counit = unwrap . linear counit'
        where counit' (Left a) = (wrap . counit) (return a)
              counit' (Right b) = (wrap . counit) (return b)
    -- counit = counit . p1 <<+>> counit . p2
    comult = linear comult' where
        comult' (Left a) = fmap (\(a1,a2) -> (Left a1, Left a2)) $ comult $ return a
        comult' (Right b) = fmap (\(b1,b2) -> (Right b1, Right b2)) $ comult $ return b
    -- comult = ( (i1 `tf` i1) . comult . p1 ) <<+>> ( (i2 `tf` i2) . comult . p2 )




-- Kassel p32
-- |The tensor product of k-algebras can itself be given the structure of a k-algebra
instance (Eq k, Num k, Ord a, Ord b, Algebra k a, Algebra k b) => Algebra k (Tensor a b) where
    -- unit 0 = V []
    unit x = x *> (unit 1 `te` unit 1)
    mult = (mult `tf` mult) . fmap (\((a,b),(a',b')) -> ((a,a'),(b,b')) )
    -- mult = linear m where
    --     m ((a,b),(a',b')) = (mult $ return (a,a')) `te` (mult $ return (b,b'))

-- Kassel p42
-- |The tensor product of k-coalgebras can itself be given the structure of a k-coalgebra
instance (Eq k, Num k, Ord a, Ord b, Coalgebra k a, Coalgebra k b) => Coalgebra k (Tensor a b) where
    counit = unwrap . linear counit'
        where counit' (a,b) = (wrap . counit . return) a * (wrap . counit . return) b -- (*) taking place in Vect k ()
    -- what this really says is that counit (a `tensor` b) = counit a * counit b
    -- counit = counit . linear (\(x,y) -> counit' (return x) * counit' (return y))
    comult = nf . fmap (\((a,a'),(b,b')) -> ((a,b),(a',b')) ) . (comult `tf` comult)
    -- comult = assocL . (id `tf` assocR) . (id `tf` (twist `tf` id))
    --        . (id `tf` assocL) . assocR . (comult `tf` comult)


-- The set coalgebra - can be defined on any set
instance (Eq k, Num k) => Coalgebra k EBasis where
    counit (V ts) = sum [x | (ei,x) <- ts]  -- trace
    comult = fmap ( \ei -> (ei,ei) )        -- diagonal

newtype SetCoalgebra b = SC b deriving (Eq,Ord,Show)

instance (Eq k, Num k) => Coalgebra k (SetCoalgebra b) where
    counit (V ts) = sum [x | (m,x) <- ts]  -- trace
    comult = fmap ( \m -> (m,m) )          -- diagonal


newtype MonoidCoalgebra m = MC m deriving (Eq,Ord,Show)

instance (Eq k, Num k, Ord m, Mon m) => Coalgebra k (MonoidCoalgebra m) where
    counit (V ts) = sum [if m == MC munit then x else 0 | (m,x) <- ts]
    comult = linear cm
        where cm m = if m == MC munit then return (m,m) else return (m, MC munit) <+> return (MC munit, m)
-- Brzezinski and Wisbauer, Corings and Comodules, p5

-- Both of the above can be used to define coalgebra structure on polynomial algebras
-- by using the definitions above on the generators (ie the indeterminates) and then extending multiplicatively
-- They are then guaranteed to be algebra morphisms?


-- MODULES AND COMODULES

class Algebra k a => Module k a m where
    action :: Vect k (Tensor a m) -> Vect k m

r *. m = action (r `te` m)

class Coalgebra k c => Comodule k c n where
    coaction :: Vect k n -> Vect k (Tensor c n)


instance Algebra k a => Module k a a where
    action = mult

instance Coalgebra k c => Comodule k c c where
    coaction = comult

-- module and comodule instances for tensor products

-- Kassel p57-8

instance (Eq k, Num k, Ord a, Ord u, Ord v, Algebra k a, Module k a u, Module k a v)
         => Module k (Tensor a a) (Tensor u v) where
    -- action x = nf $ x >>= action'
    action = linear action'
        where action' ((a,a'), (u,v)) = (action $ return (a,u)) `te` (action $ return (a',v))

instance (Eq k, Num k, Ord a, Ord u, Ord v, Bialgebra k a, Module k a u, Module k a v)
         => Module k a (Tensor u v) where
    -- action x = nf $ x >>= action'
    action = linear action'
        where action' (a,(u,v)) = action $ (comult $ return a) `te` (return (u,v))
-- !! Overlapping instances
-- If a == Tensor b b, then we have overlapping instance with the previous definition
-- On the other hand, if a == Tensor u v, then we have overlapping instance with the earlier instance

-- Kassel p63
instance (Eq k, Num k, Ord a, Ord m, Ord n, Bialgebra k a, Comodule k a m, Comodule k a n)
         => Comodule k a (Tensor m n) where
    coaction = (mult `tf` id) . twistm . (coaction `tf` coaction)
        where twistm x = nf $ fmap ( \((h,m), (h',n)) -> ((h,h'), (m,n)) ) x


-- PAIRINGS

-- |A pairing is a non-degenerate bilinear form U x V -> k.
-- We are typically interested in pairings having additional properties. For example:
--
-- * A bialgebra pairing is a pairing between bialgebras A and B such that the mult in A is adjoint to the comult in B, and vice versa, and the unit in A is adjoint to the counit in B, and vice versa.
--
-- * A Hopf pairing is a bialgebra pairing between Hopf algebras A and B such that the antipodes in A and B are adjoint.
class HasPairing k u v where
    pairing :: Vect k (Tensor u v) -> Vect k ()

-- |The pairing function with a more Haskellish type signature
pairing' :: (Num k, HasPairing k u v) => Vect k u -> Vect k v -> k
pairing' u v = unwrap (pairing (u `te` v))

instance (Eq k, Num k) => HasPairing k () () where
    pairing = mult

instance (Eq k, Num k, HasPairing k u v, HasPairing k u' v') => HasPairing k (Tensor u u') (Tensor v v') where
    pairing = mult . (pairing `tf` pairing) . fmap (\((u,u'),(v,v')) -> ((u,v),(u',v')))
    -- pairing = fmap (\((),()) -> ()) . (pairing `tf` pairing) . fmap (\((u,u'),(v,v')) -> ((u,v),(u',v')))

