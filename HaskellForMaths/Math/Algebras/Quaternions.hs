-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, NoMonomorphismRestriction #-}

-- |A module defining the algebra of quaternions over an arbitrary field.
--
-- The quaternions are the algebra defined by the basis {1,i,j,k}, where i^2 = j^2 = k^2 = ijk = -1
module Math.Algebras.Quaternions where

import Math.Core.Field
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures


-- Conway & Smith, On Quaternions and Octonions

-- QUATERNIONS

data HBasis = One | I | J | K deriving (Eq,Ord)

type Quaternion k = Vect k HBasis

instance Show HBasis where
    show One = "1"
    show I = "i"
    show J = "j"
    show K = "k"

instance (Eq k, Num k) => Algebra k HBasis where
    unit x = x *> return One
    mult = linear mult'
         where mult' (One,b) = return b
               mult' (b,One) = return b
               mult' (I,I) = unit (-1)
               mult' (J,J) = unit (-1)
               mult' (K,K) = unit (-1)
               mult' (I,J) = return K
               mult' (J,I) = -1 *> return K
               mult' (J,K) = return I
               mult' (K,J) = -1 *> return I
               mult' (K,I) = return J
               mult' (I,K) = -1 *> return J

-- |The quaternions have {1,i,j,k} as basis, where i^2 = j^2 = k^2 = ijk = -1.
i,j,k :: Num k => Quaternion k
i = return I
j = return J
k = return K


class Algebra k a => HasConjugation k a where
    -- |A conjugation operation is required to satisfy the following laws:
    --
    -- * conj (x+y) = conj x + conj y
    --
    -- * conj (x*y) = conj y * conj x  (note the order-reversal)
    --
    -- * conj (conj x) = x
    --
    -- * conj x = x if and only if x in k
    conj :: Vect k a -> Vect k a
    -- |The squared norm is defined as sqnorm x = x * conj x. It satisfies:
    --
    -- * sqnorm (x*y) = sqnorm x * sqnorm y
    --
    -- * sqnorm (unit k) = k^2, for k a scalar
    sqnorm :: Vect k a -> k

-- |If an algebra has a conjugation operation, then it has multiplicative inverses,
-- via 1/x = conj x / sqnorm x
instance (Eq k, Fractional k, Ord a, Show a, HasConjugation k a) => Fractional (Vect k a) where
    recip 0 = error "recip 0"
    recip x = (1 / sqnorm x) *> conj x
    fromRational q = fromRational q *> 1

-- |The scalar part of the quaternion w+xi+yj+zk is w. Also called the real part.
scalarPart :: (Num k) => Quaternion k -> k
scalarPart = coeff One

-- |The vector part of the quaternion w+xi+yj+zk is xi+yj+zk. Also called the pure part.
vectorPart :: (Eq k, Num k) => Quaternion k -> Quaternion k
vectorPart q = q - scalarPart q *> 1

instance (Eq k, Num k) => HasConjugation k HBasis where
    conj = (>>= conj') where
        conj' One = return One
        conj' imag = -1 *> return imag
    -- ie conj = linear conj', but avoiding unnecessary nf call
    sqnorm x = sum $ map ((^2) . snd) $ terms x
    -- sqnorm x = scalarPart (conj x * x) -- the vector part will be zero anyway
    -- sqnorm x = x <.> x
{-
instance Fractional k => Fractional (Quaternion k) where
    recip 0 = error "Quaternion.recip 0"
    recip x = (1 / sqnorm x) *> conj x
    fromRational q = fromRational q *> 1
-}

x <.> y = scalarPart (conj x * y)
-- x <..> y = 1/2 * (sqnorm x + sqnorm y - sqnorm (x-y))



x^-1 = recip x

-- Conway p40
refl q = \x -> -q * conj x * q

-- Given a linear function f on the quaternions, return the matrix representing it,
-- relative to a given basis. The matrix is considered as acting on the right.
asMatrix f bs = [ let fi = f ei in [ej <.> fi | ej <- bs] | ei <- bs  ]
-- It is possible to write this function using coeff, instead of <.>,
-- but then you have to pass in I,J,K, instead of i,j,k, which is uglier.

-- Conway p24
-- A homomorphism from H\0 to SO3
-- if q is restricted to unit quaternions, this is a double cover of SO3 (since q, -q induce same rotation)
-- The unit quaternions form the group Spin3
reprSO3' q = \x -> q^-1 * x * q

-- |Given a non-zero quaternion q in H, the map x -> q^-1 * x * q defines an action on the 3-dimensional vector space
-- of pure quaternions X (ie linear combinations of i,j,k). It turns out that this action is a rotation of X,
-- and this is a surjective group homomorphism from H* onto SO3. If we restrict q to the group of unit quaternions
-- (those of norm 1), then this homomorphism is 2-to-1 (since q and -q give the same rotation).
-- This shows that the multiplicative group of unit quaternions is isomorphic to Spin3, the double cover of SO3.
--
-- @reprSO3 q@ returns the 3*3 matrix representing this map.
reprSO3 :: (Eq k, Fractional k) => Quaternion k -> [[k]]
reprSO3 q = reprSO3' q `asMatrix` [i,j,k]
-- It's clear from the definition that repr3' q leaves scalars invariant

-- for achiral elts, ie GO3\SO3, we compose the above with conj

-- For unit quaternions, this is a double cover of SO4 (since (l,r), (-l,-r) induce same rotation)
-- Ordered pairs of unit quaternions form the group Spin4
reprSO4' (l,r) = \x -> l^-1 * x * r
-- then (l1,r1) * (l2,r2) -> (l1*l2,r1*r2)
-- having l^-1 is required for this to work

-- |Given a pair of unit quaternions (l,r), the map x -> l^-1 * x * r defines an action on the 4-dimensional space
-- of quaternions. It turns out that this action is a rotation, and this is a surjective group homomorphism
-- onto SO4. The homomorphism is 2-to-1 (since (l,r) and (-l,-r) give the same map).
-- This shows that the multiplicative group of pairs of unit quaternions (with pointwise multiplication)
-- is isomorphic to Spin4, the double cover of SO4.
--
-- @reprSO4 (l,r)@ returns the 4*4 matrix representing this map.
reprSO4 :: (Eq k, Fractional k) => (Quaternion k, Quaternion k) -> [[k]]
reprSO4 (l,r) = reprSO4' (l,r) `asMatrix` [1,i,j,k]
-- could consider checking that l,r are unit length - except that this is hard to achieve working over Q

reprSO4d lr = reprSO4 (p1 lr, p2 lr)

-- for achiral elts, GO4\SO4, we compose the above with conj


-- DUAL SPACE OF QUATERNIONS AS COALGEBRA

one',i',j',k' :: Num k => Vect k (Dual HBasis)
one' = return (Dual One)
i' = return (Dual I)
j' = return (Dual J)
k' = return (Dual K)

-- Coalgebra structure on the dual vector space to the quaternions
-- The comult is the transpose of mult
instance (Eq k, Num k) => Coalgebra k (Dual HBasis) where
    counit = unwrap . linear counit'
        where counit' (Dual One) = return ()
              counit' _ = zero
    comult = linear comult'
        where comult' (Dual One) = return (Dual One, Dual One) <+>
                  (-1) *> ( return (Dual I, Dual I) <+> return (Dual J, Dual J) <+> return (Dual K, Dual K) )
              comult' (Dual I) = return (Dual One, Dual I) <+> return (Dual I, Dual One) <+>
                  return (Dual J, Dual K) <+> (-1) *> return (Dual K, Dual J)
              comult' (Dual J) = return (Dual One, Dual J) <+> return (Dual J, Dual One) <+>
                  return (Dual K, Dual I) <+> (-1) *> return (Dual I, Dual K)
              comult' (Dual K) = return (Dual One, Dual K) <+> return (Dual K, Dual One) <+>
                  return (Dual I, Dual J) <+> (-1) *> return (Dual J, Dual I)

{-
-- Of course, we can define this coalgebra structure on the quaternions themselves
-- However, it is not compatible with the algebra structure: we don't get a bialgebra
instance Num k => Coalgebra k HBasis where
    counit = unwrap . linear counit'
        where counit' One = return ()
              counit' _ = zero
    comult = linear comult'
        where comult' One = return (One,One) <+> (-1) *> ( return (I,I) <+> return (J,J) <+> return (K,K) )
              comult' I = return (One,I) <+> return (I,One) <+> return (J,K) <+> (-1) *> return (K,J)
              comult' J = return (One,J) <+> return (J,One) <+> return (K,I) <+> (-1) *> return (I,K)
              comult' K = return (One,K) <+> return (K,One) <+> return (I,J) <+> (-1) *> return (J,I)
-}

{-
-- Set coalgebra instance
instance Num k => Coalgebra k HBasis where
    counit (V ts) = sum [x | (m,x) <- ts] -- trace
    comult = fmap (\m -> T m m)           -- diagonal
-}

{-
instance Num k => Coalgebra k HBasis where
    counit (V ts) = sum [x | (One,x) <- ts]
    comult = linear cm
        where cm m = if m == One then return (m,m) else return (m,One) <+> return (One,m)
-}
