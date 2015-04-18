-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}

-- |A module defining direct sum and tensor product of vector spaces
module Math.Algebras.TensorProduct where

import Math.Algebras.VectorSpace

infix 7 `te`, `tf`
infix 6 `dsume`, `dsumf`


-- DIRECT SUM

-- |A type for constructing a basis for the direct sum of vector spaces.
-- The direct sum of Vect k a and Vect k b is Vect k (DSum a b)
type DSum a b = Either a b

-- |Injection of left summand into direct sum
i1 :: Vect k a -> Vect k (DSum a b)
i1 = fmap Left

-- |Injection of right summand into direct sum
i2 :: Vect k b -> Vect k (DSum a b)
i2 = fmap Right

-- |The coproduct of two linear functions (with the same target).
-- Satisfies the universal property that f == coprodf f g . i1 and g == coprodf f g . i2
coprodf :: (Eq k, Num k, Ord t) =>
    (Vect k a -> Vect k t) -> (Vect k b -> Vect k t) -> Vect k (DSum a b) -> Vect k t
coprodf f g = linear fg' where
    fg' (Left a) = f (return a)
    fg' (Right b) = g (return b)


-- |Projection onto left summand from direct sum
p1 :: (Eq k, Num k, Ord a) => Vect k (DSum a b) -> Vect k a
p1 = linear p1' where
    p1' (Left a) = return a
    p1' (Right b) = zero

-- |Projection onto right summand from direct sum
p2 :: (Eq k, Num k, Ord b) => Vect k (DSum a b) -> Vect k b
p2 = linear p2' where
    p2' (Left a) = zero
    p2' (Right b) = return b

-- |The product of two linear functions (with the same source).
-- Satisfies the universal property that f == p1 . prodf f g and g == p2 . prodf f g
prodf :: (Eq k, Num k, Ord a, Ord b) =>
    (Vect k s -> Vect k a) -> (Vect k s -> Vect k b) -> Vect k s -> Vect k (DSum a b)
prodf f g = linear fg' where
    fg' b = fmap Left (f $ return b) <+> fmap Right (g $ return b)


-- |The direct sum of two vector space elements
dsume :: (Eq k, Num k, Ord a, Ord b) => Vect k a -> Vect k b -> Vect k (DSum a b)
-- dsume x y = fmap Left x <+> fmap Right y
dsume x y = i1 x <+> i2 y

-- |The direct sum of two linear functions.
-- Satisfies the universal property that f == p1 . dsumf f g . i1 and g == p2 . dsumf f g . i2
dsumf :: (Eq k, Num k, Ord a, Ord b, Ord a', Ord b') => 
    (Vect k a -> Vect k a') -> (Vect k b -> Vect k b') -> Vect k (DSum a b) -> Vect k (DSum a' b')
dsumf f g ab = (i1 . f . p1) ab <+> (i2 . g . p2) ab


-- TENSOR PRODUCT

-- |A type for constructing a basis for the tensor product of vector spaces.
-- The tensor product of Vect k a and Vect k b is Vect k (Tensor a b)
type Tensor a b = (a,b)

-- |The tensor product of two vector space elements
te :: Num k => Vect k a -> Vect k b -> Vect k (Tensor a b)
te (V us) (V vs) = V [((a,b), x*y) | (a,x) <- us, (b,y) <- vs]
-- te (V us) (V vs) = V [((ei,ej), xi*xj) | (ei,xi) <- us, (ej,xj) <- vs]
-- preserves order - that is, if the inputs are correctly ordered, so is the output

-- Implicit assumption - f and g are linear
-- |The tensor product of two linear functions
tf :: (Eq k, Num k, Ord a', Ord b') => (Vect k a -> Vect k a') -> (Vect k b -> Vect k b')
   -> Vect k (Tensor a b) -> Vect k (Tensor a' b')
tf f g (V ts) = sum [x *> te (f $ return a) (g $ return b) | ((a,b), x) <- ts]
    where sum = foldl add zero -- (V [])


-- tensor isomorphisms

-- in fact, this definition works for any Functor f, not just (Vect k)
assocL :: Vect k (Tensor a (Tensor b c)) -> Vect k (Tensor (Tensor a b) c)
assocL = fmap ( \(a,(b,c)) -> ((a,b),c) )

assocR :: Vect k (Tensor (Tensor a b) c) -> Vect k (Tensor a (Tensor b c))
assocR = fmap ( \((a,b),c) -> (a,(b,c)) )

unitInL :: Vect k a -> Vect k (Tensor () a)
unitInL = fmap ( \a -> ((),a) )

unitOutL :: Vect k (Tensor () a) -> Vect k a
unitOutL = fmap ( \((),a) -> a )

unitInR :: Vect k a -> Vect k (Tensor a ())
unitInR = fmap ( \a -> (a,()) )

unitOutR :: Vect k (Tensor a ()) -> Vect k a
unitOutR = fmap ( \(a,()) -> a )

twist :: (Eq k, Num k, Ord a, Ord b) => Vect k (Tensor a b) -> Vect k (Tensor b a)
twist v = nf $ fmap ( \(a,b) -> (b,a) ) v
-- note the nf call, as f is not order-preserving


distrL :: (Eq k, Num k, Ord a, Ord b, Ord c)
    => Vect k (Tensor a (DSum b c)) -> Vect k (DSum (Tensor a b) (Tensor a c))
distrL v = nf $ fmap (\(a,bc) -> case bc of Left b -> Left (a,b); Right c -> Right (a,c)) v

undistrL :: (Eq k, Num k, Ord a, Ord b, Ord c)
    => Vect k (DSum (Tensor a b) (Tensor a c)) -> Vect k (Tensor a (DSum b c))
undistrL v = nf $ fmap ( \abc -> case abc of Left (a,b) -> (a,Left b); Right (a,c) -> (a,Right c) ) v

distrR :: Vect k (Tensor (DSum a b) c) -> Vect k (DSum (Tensor a c) (Tensor b c))
distrR v = fmap ( \(ab,c) -> case ab of Left a -> Left (a,c); Right b -> Right (b,c) ) v
-- order-preserving, so no nf call needed

undistrR :: Vect k (DSum (Tensor a c) (Tensor b c)) -> Vect k (Tensor (DSum a b) c)
undistrR v = fmap ( \abc -> case abc of Left (a,c) -> (Left a, c); Right (b,c) -> (Right b, c) ) v

-- For example:
-- > distrL (e1 `te` i1 e2) :: Vect Q (DSum (Tensor EBasis EBasis) (Tensor EBasis EBasis))
-- Left (e1,e2)


ev :: (Eq k, Num k, Ord b) => Vect k (Tensor (Dual b) b) -> k
ev = unwrap . linear (\(Dual bi, bj) -> delta bi bj *> return ())
-- slightly cheating, as delta i j is meant to compare indices, not the basis elements themselves

delta i j = if i == j then 1 else 0

reify :: (Eq k, Num k, Ord b) => Vect k (Dual b) -> (Vect k b -> k)
reify f x = ev (f `te` x)
