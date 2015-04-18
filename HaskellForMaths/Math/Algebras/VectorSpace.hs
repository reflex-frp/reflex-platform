-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK prune #-}

-- |A module defining the type and operations of free k-vector spaces over a basis b (for a field k)
module Math.Algebras.VectorSpace where

import qualified Data.List as L
import qualified Data.Set as S -- only needed for toSet

infixr 7 *>
infixl 7 <*
infixl 6 <+>, <->


-- |Given a field type k and a basis type b, Vect k b is the type of the free k-vector space over b.
-- Elements (values) of Vect k b consist of k-linear combinations of elements (values) of b.
--
-- In order for Vect k b to be a vector space, it is necessary that k is a field (that is, an instance of Fractional).
-- In practice, we often relax this condition, and require that k is a ring (that is, an instance of Num). In that case,
-- Vect k b should more correctly be called (the type of) the free k-module over b.
--
-- Most of the code requires that b is an instance of Ord. This is primarily to enable us to simplify to a normal form.
newtype Vect k b = V [(b,k)] deriving (Eq,Ord)

instance (Show k, Eq k, Num k, Show b) => Show (Vect k b) where
    show (V []) = "0"
    show (V ts) = concatWithPlus $ map showTerm ts
        where showTerm (b,x) | show b == "1" = show x
                             | show x == "1" = show b
                             | show x == "-1" = "-" ++ show b
                             | otherwise = (if isAtomic (show x) then show x else "(" ++ show x ++ ")") ++
                                           show b
                                           -- (if ' ' `notElem` show b then show b else "(" ++ show b ++ ")")
                                           -- if we put this here we miss the two cases above
              concatWithPlus (t1:t2:ts) = if head t2 == '-'
                                          then t1 ++ concatWithPlus (t2:ts)
                                          else t1 ++ '+' : concatWithPlus (t2:ts)
              concatWithPlus [t] = t
              isAtomic (c:cs) = isAtomic' cs
              isAtomic' ('^':'-':cs) = isAtomic' cs
              isAtomic' ('+':cs) = False
              isAtomic' ('-':cs) = False
              isAtomic' (c:cs) = isAtomic' cs
              isAtomic' [] = True

terms (V ts) = ts

-- |Return the coefficient of the specified basis element in a vector
coeff b v = sum [k | (b',k) <- terms v, b' == b]

-- |Remove the term for a specified basis element from a vector
removeTerm b v = v <-> coeff b v *> return b

-- Deprecated
zero = V []

-- |The zero vector
zerov :: Vect k b
zerov = V []

-- |Addition of vectors
add :: (Ord b, Eq k, Num k) => Vect k b -> Vect k b -> Vect k b
add (V ts) (V us) = V $ addmerge ts us

-- |Addition of vectors (same as add)
(<+>) :: (Ord b, Eq k, Num k) => Vect k b -> Vect k b -> Vect k b
(<+>) = add

addmerge ((a,x):ts) ((b,y):us) =
    case compare a b of
    LT -> (a,x) : addmerge ts ((b,y):us)
    EQ -> if x+y == 0 then addmerge ts us else (a,x+y) : addmerge ts us
    GT -> (b,y) : addmerge ((a,x):ts) us
addmerge ts [] = ts
addmerge [] us = us

-- |Sum of a list of vectors
sumv :: (Ord b, Eq k, Num k) => [Vect k b] -> Vect k b
sumv = foldl (<+>) zerov

-- Deprecated
neg (V ts) = V $ map (\(b,x) -> (b,-x)) ts

-- |Negation of a vector
negatev :: (Eq k, Num k) => Vect k b -> Vect k b
negatev (V ts) = V $ map (\(b,x) -> (b,-x)) ts

-- |Subtraction of vectors
(<->) :: (Ord b, Eq k, Num k) => Vect k b -> Vect k b -> Vect k b
(<->) u v = u <+> negatev v

-- |Scalar multiplication (on the left)
smultL :: (Eq k, Num k) => k -> Vect k b -> Vect k b
smultL 0 _ = zero -- V []
smultL k (V ts) = V [(ei,k*xi) | (ei,xi) <- ts]

-- |Same as smultL. Mnemonic is \"multiply through (from the left)\"
(*>) :: (Eq k, Num k) => k -> Vect k b -> Vect k b
(*>) = smultL

-- |Scalar multiplication on the right
smultR :: (Eq k, Num k) => Vect k b -> k -> Vect k b
smultR _ 0 = zero -- V []
smultR (V ts) k = V [(ei,xi*k) | (ei,xi) <- ts]

-- |Same as smultR. Mnemonic is \"multiply through (from the right)\"
(<*) :: (Eq k, Num k) => Vect k b -> k -> Vect k b
(<*) = smultR

-- same as return
-- injection of basis elt into vector space
-- inject b = V [(b,1)]

-- same as fmap
-- liftFromBasis f (V ts) = V [(f b, x) | (b, x) <- ts]
-- if f is not order-preserving, then you need to call nf afterwards

-- |Convert an element of Vect k b into normal form. Normal form consists in having the basis elements in ascending order,
-- with no duplicates, and all coefficients non-zero
nf :: (Ord b, Eq k, Num k) => Vect k b -> Vect k b
nf (V ts) = V $ nf' $ L.sortBy compareFst ts where
    nf' ((b1,x1):(b2,x2):ts) =
        case compare b1 b2 of
        LT -> if x1 == 0 then nf' ((b2,x2):ts) else (b1,x1) : nf' ((b2,x2):ts)
        EQ -> if x1+x2 == 0 then nf' ts else nf' ((b1,x1+x2):ts)
        GT -> error "nf': not pre-sorted"
    nf' [(b,x)] = if x == 0 then [] else [(b,x)]
    nf' [] = []
    compareFst (b1,x1) (b2,x2) = compare b1 b2
    -- compareFst = curry ( uncurry compare . (fst *** fst) )


-- |Given a field k, (Vect k) is a functor, the \"free k-vector space\" functor.
--
-- In the mathematical sense, this can be regarded as a functor from the category Set (of sets) to the category k-Vect
-- (of k-vector spaces). In Haskell, instead of Set we have Hask, the category of Haskell types. However, for our purposes
-- it is helpful to identify Hask with Set, but identifying a Haskell type with its set of inhabitants.
--
-- The type constructor (Vect k) gives the action of the functor on objects in the category,
-- taking a set (type) to a free k-vector space. fmap gives the action of the functor on arrows in the category,
-- taking a function between sets (types) to a linear map between vector spaces.
--
-- Note that if f is not order-preserving, then (fmap f) is not guaranteed to return results in normal form,
-- so it may be preferable to use (nf . fmap f).
instance Functor (Vect k) where
    -- lift a function on the basis to a function on the vector space
    fmap f (V ts) = V [(f b, x) | (b,x) <- ts]
-- Note that if f is not order-preserving, then we need to call "nf" afterwards

-- |Given a field k, the type constructor (Vect k) is a monad, the \"free k-vector space monad\".
--
-- In order to understand this, it is probably easiest to think of a free k-vector space as a kind of container,
-- a bit like a list, except that order doesn't matter, and you're allowed arbitrary (even negative or fractional)
-- quantities of the basis elements in the container.
--
-- According to this way of thinking, return is the function that puts a basis element into the vector space (container).
--
-- Given a function f from the basis of one vector space to another vector space (a -> Vect k b),
-- bind (>>=) lifts it to a function (>>= f) from the first vector space to the second (Vect k a -> Vect k b).
--
-- Note that in general (>>= f) applied to a vector will not return a result in normal form,
-- so it is usually preferable to use (linear f) instead.
instance Num k => Monad (Vect k) where
    return a = V [(a,1)]
    V ts >>= f = V $ concat [ [(b,y*x) | let V us = f a, (b,y) <- us] | (a,x) <- ts]
    -- Note that as we can't assume Ord a in the Monad instance, we need to call "nf" afterwards

-- |A linear map between vector spaces A and B can be defined by giving its action on the basis elements of A.
-- The action on all elements of A then follows by linearity.
--
-- If we have A = Vect k a, B = Vect k b, and f :: a -> Vect k b is a function from the basis elements of A into B,
-- then @linear f@ is the linear map that this defines by linearity.
linear :: (Ord b, Eq k, Num k) => (a -> Vect k b) -> Vect k a -> Vect k b
linear f v = nf $ v >>= f

newtype EBasis = E Int deriving (Eq,Ord)

instance Show EBasis where show (E i) = "e" ++ show i

e i = return $ E i
e1 = e 1
e2 = e 2
e3 = e 3

-- dual (E i) = E (-i)


-- |Trivial k is the field k considered as a k-vector space. In maths, we would not normally make a distinction here,
-- but in the code, we need this if we want to be able to put k as one side of a tensor product.
type Trivial k = Vect k ()

-- |Wrap an element of the field k to an element of the trivial k-vector space
wrap :: (Eq k, Num k) => k -> Vect k ()
wrap 0 = zero
wrap x = V [( (),x)]

-- |Unwrap an element of the trivial k-vector space to an element of the field k
unwrap :: Num k => Vect k () -> k
unwrap (V []) = 0
unwrap (V [( (),x)]) = x

-- |Given a finite vector space basis b, Dual b can be used to represent a basis for the dual vector space.
-- The intention is that for a given individual basis element b_i, (Dual b_i) represents the indicator function for b_i,
-- which takes b_i to 1 and all other basis elements to 0.
--
-- (Note that if the basis b is infinite, then Dual b may only represent a sub-basis of the dual vector space.)
newtype Dual b = Dual b deriving (Eq,Ord)

instance Show basis => Show (Dual basis) where
    show (Dual b) = show b ++ "'"


e' i = return $ Dual $ E i
e1' = e' 1
e2' = e' 2
e3' = e' 3

dual :: Vect k b -> Vect k (Dual b)
dual = fmap Dual


(f <<+>> g) v = f v <+> g v

zerof v = zerov

sumf fs = foldl (<<+>>) zerof fs
