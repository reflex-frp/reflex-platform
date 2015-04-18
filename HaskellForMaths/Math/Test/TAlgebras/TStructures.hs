-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, RankNTypes, MultiParamTypeClasses #-}


module Math.Test.TAlgebras.TStructures where

-- import Test.QuickCheck
-- don't actually need, as we don't define any Arbitrary instances here

import Control.Arrow ( (>>>) ) -- actually you can get this from Category?


import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures -- what we're testing



prop_Linear f (k,l,x,y) =
    f (add (smultL k x) (smultL l y)) == add (smultL k (f x)) (smultL l (f y))
-- now use this to show algebra and coalgebra ops are linear

-- in this version we supply z of the intended return type of f,
-- so that we can make sure we select the correct instance for f polymorphic in return type
prop_Linear' f (k,l,x,y,z) =
    f (add (smultL k x) (smultL l y)) `add` z == add (smultL k (f x)) (smultL l (f y)) `add` z


-- prop_Bilinear could be defined in terms of prop_Linear over tensor product
-- if we had a way to convert a bilinear function to a tensor function

prop_Algebra_Linear ::
    (Eq k, Num k, Ord b, Algebra k b) =>
    (k, k, k, k, Vect k b, Vect k b, Vect k b, Vect k b) -> Bool
prop_Algebra_Linear (k,l,m,n,x,y,z,w) =
--    (unit (k * m + l * n) :: Vect k b) == (add (smultL k (unit m)) (smultL l (unit n)) :: Vect k b) &&
    prop_Linear' unit' (k,l,wrap m, wrap n, x) &&
    prop_Linear mult (k,l, x `te` y, z `te` w)
    where wrap = (\c -> V [((),c)]) :: k -> Trivial k

prop_Coalgebra_Linear (k,l,x,y) =
    prop_Linear counit' (k,l,x,y) &&
    prop_Linear comult (k,l,x,y)


-- ALGEBRAS

prop_Algebra (k,x,y,z) =
    (mult . (id `tf` mult)) (x `te` (y `te` z)) ==
         (mult . (mult `tf` id)) ((x `te` y) `te` z)              && -- associativity
    unitOutL (k' `te` x) == (mult . (unit' `tf` id)) (k' `te` x)  && -- left unit
    unitOutR (x `te` k') == (mult . (id `tf` unit')) (x `te` k')     -- right unit
    -- mult (x `te` mult (y `te` z)) == mult (mult (x `te` y) `te` z)  && -- associativity
    -- smultL k x == mult (unit k `te` x)                              && -- left unit
    -- smultR x k == mult (x `te` unit k)                           -- && -- right unit
    where k' = k *> return ()
-- additionally, unit and mult must be linear

prop_Commutative (x,y) =
    let xy = x `te` y
    in (mult . twist) xy == mult xy

prop_Algebra_DSum (k,(a1,a2,a3),(b1,b2,b3)) = prop_Algebra (k, a1 `dsume` b1, a2 `dsume` b2, a3 `dsume` b3)

prop_Algebra_TProd (k,(a1,a2,a3),(b1,b2,b3)) = prop_Algebra (k, a1 `te` b1, a2 `te` b2, a3 `te` b3)


-- COALGEBRAS

prop_Coalgebra x =
    ((comult `tf` id) . comult) x == (assocL . (id `tf` comult) . comult) x && -- coassociativity
    ((counit' `tf` id) . comult) x == unitInL x                             && -- left counit
    ((id `tf` counit') . comult) x == unitInR x                                -- right counit
-- additionally, counit and comult must be linear

prop_Cocommutative x =
    (twist . comult) x == comult x


-- MORPHISMS

prop_AlgebraMorphism f (k,x,y) =
    (f . unit) k == unit k &&
    (f . mult) (x `te` y) == (mult . (f `tf` f)) (x `te` y) 

-- in this version we supply z of the intended return type of f,
-- so that we can make sure we select the correct instance for f polymorphic in return type
prop_AlgebraMorphism' f (k,l,x,y,z) =
    (f . unit) k + z == unit k + z &&
    (f . mult) (x `te` y) == (mult . (f `tf` f)) (x `te` y) 

prop_CoalgebraMorphism f x =
    (counit . f) x == counit x &&
    ( (f `tf` f) . comult) x == (comult . f) x

-- An antihomomorphism is like a homomorphism except that it reverses the order of multiplication
prop_AlgebraAntiMorphism f (k,x,y) =
    (f . unit) k == unit k &&
    (f . mult) (x `te` y) == (mult . (f `tf` f) . twist) (x `te` y) 

prop_CoalgebraAntiMorphism f x =
    (counit . f) x == counit x &&
    (twist . (f `tf` f) . comult) x == (comult . f) x

prop_HopfAlgebraMorphism f x = (f . antipode) x == (antipode . f) x


-- BIALGEBRAS
{-
prop_Bialgebra1 (x,y) =
    let xy = x `te` y in
    (comult . mult) xy ==
    ( (mult `tf` mult) .
      assocL . (id `tf` assocR) .
      (id `tf` (twist `tf` id)) .
      (id `tf` assocL) . assocR .
      (comult `tf` comult) ) xy
-}
prop_Bialgebra1 (x,y) =
    let xy = x `te` y in
    (comult . mult) xy ==
    ( (mult `tf` mult) .
      fmap (\((a,a'),(b,b')) -> ((a,b),(a',b')) ) .
      (comult `tf` comult) ) xy

prop_Bialgebra2 (k,xy) =
    (comult . unit') k' + xy == ((unit' `tf` unit') . iso) k' + xy
    where iso = fmap (\ () -> ((),()) ) -- the isomorphism k ~= k tensor k
          k' = wrap k -- inject into the trivial algebra
-- the +xy is just to force the other expression to be of the right type

prop_Bialgebra3 (x,y) =
    (counit' . mult) xy == (iso . (counit' `tf` counit')) xy
    where xy = x `te` y
          iso = fmap ( \((),()) -> ())

prop_Bialgebra4 (k,x) =
    id k == (counit . (\a -> a+x-x) . unit) k
-- so we are using the x just to force the intermediate value to be of the right type

prop_Bialgebra (k,x,y) =
    prop_Bialgebra1 (x,y) &&
    prop_Bialgebra2 (k,x `te` y) &&
    prop_Bialgebra3 (x,y) &&
    prop_Bialgebra4 (k,x)

-- Claim that this is equivalent to the above, but much shorter because it piggy-backs on
-- the coalgebra instance for tensor product, and the algebra morphism definition
prop_BialgebraA (k,x,y) = prop_AlgebraMorphism (wrap . counit) (k,x,y) && prop_AlgebraMorphism comult (k,x,y)


-- Need a way to force the result type of (unit . unwrap) to be the same as the type of x and y
-- prop_BialgebraC (k,x,y) = prop_CoalgebraMorphism (unit . unwrap) (wrap k) && prop_CoalgebraMorphism mult (x `te` y)


prop_HopfAlgebra x =
    (unit . counit) x == (mult . (antipode `tf` id) . comult) x &&
    (unit . counit) x == (mult . (id `tf` antipode) . comult) x

-- Street p87
-- we also require that f be invertible
prop_YangBaxter f (x,y,z) =
    ( (f `tf` id) >>> assocR >>> (id `tf` f) >>> assocL >>> (f `tf` id) >>> assocR ) xyz == 
    ( assocR >>> (id `tf` f) >>> assocL >>> (f `tf` id) >>> assocR >>> (id `tf` f) ) xyz
    where xyz = ( (x `te` y) `te` z) 



-- MODULES AND COMODULES

prop_Module_Linear (k,l,x,y) = prop_Linear action (k,l,x,y)

prop_Module_Assoc (r,s,m) =
    (action . (mult `tf` id)) ((r `te` s) `te` m) == (action . (id `tf` action)) (r `te` (s `te` m))
{-
prop_Module_Unit (k,m) =
    (action . (unit' `tf` id)) k' ==  
-}


-- PAIRINGS

-- http://mathoverflow.net/questions/20666/is-a-bialgebra-pairing-of-hopf-algebras-automatically-a-hopf-pairing
-- Hazewinkel p155
-- Majid, Quantum Groups Primer, p12
prop_BialgebraPairing
  :: (Eq k, Num k, Ord a, Ord b, Show a, Show b, Bialgebra k a,
      Bialgebra k b, HasPairing k a b) =>
     (Vect k a, Vect k a, Vect k b, Vect k b) -> Bool
prop_BialgebraPairing (u,v,x,y) =
    pairing' (mult (u `te` v)) x == pairing' (u `te` v) (comult x) && -- mult in A is adjoint to comult in B
    pairing' (comult u) (x `te` y) == pairing' u (mult (x `te` y)) && -- comult in A is adjoint to mult in B
    pairing' (1+u-u) x == counit x && -- unit (ie 1) is adjoint to counit
    pairing' u (1+x-x) == counit u
-- The +x-x is to coerce the type of unit k
-- The same could probably be achieved with ScopedTypeVariables

prop_HopfPairing (u,v,x,y) =
    prop_BialgebraPairing (u,v,x,y) &&
    pairing' (antipode u) x == pairing' u (antipode x)


-- ALTERNATIVE DEFINITION OF ALGEBRA

type TensorProd k u v =
    (u ~ Vect k a, v ~ Vect k b) => Vect k (Tensor a b)

class Algebra2 k a where
    unit2 :: k -> a
    mult2 :: TensorProd k a a -> a


-- FROBENIUS ALGEBRAS

frobeniusLeft1 = (id `tf` mult) . assocR . (comult `tf` id)

frobeniusLeft2 x = nf $ x >>= fl
    where fl (i,j) = do
              (k,l) <- comultM i
              m <- idM j
              p <- idM k
              q <- multM (l,m)
              return (p,q)

frobeniusMiddle1 = comult . mult

frobeniusMiddle2 x = nf $ x >>= fm
    where fm (i,j) = do
              k <- multM (i,j)
              (l,m) <- comultM k
              return (l,m)

prop_FrobeniusRelation (x,y) =
    let xy = x `te` y
    in frobeniusLeft1 xy == frobeniusMiddle1 xy

-- (inject == return)

multM = mult . return -- inject
comultM = comult . return -- inject
idM = id . return

-- can we do the same with unit, counit?
-- unit takes k as input, so isn't in the monad
-- counit gives k as output - what would we do with it
-- so perhaps we have to use unit' and counit'
