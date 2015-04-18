-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}

-- |A module defining the algebra of commutative polynomials over a field k.
-- Polynomials are represented as the free k-vector space with the monomials as basis.
-- 
-- A monomial ordering is required to specify how monomials are to be ordered.
-- The Lex, Glex, and Grevlex monomial orders are defined, with the possibility to add others.
--
-- In order to make use of this module, some variables must be defined, for example:
--
-- > [t,u,v,x,y,z] = map glexvar ["t","u","v","x","y","z"]
module Math.CommutativeAlgebra.Polynomial where

import Math.Core.Field
import Math.Core.Utils (toSet)
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures

-- |In order to work with monomials, we need to be able to multiply them and divide them.
-- Multiplication is defined by the Mon (monoid) class. Division is defined in this class.
-- The functions here are primarily intended for internal use only.
class (Eq m, Show m, Mon m) => Monomial m where
    mdivides :: m -> m -> Bool
    mdiv :: m -> m -> m
    mgcd :: m -> m -> m
    mlcm :: m -> m -> m
    mcoprime :: m -> m -> Bool
    mdeg :: m -> Int

-- mlcm m1 m2 = let m = mgcd m1 m2 in mmult m1 (mdiv m2 m)

mproperlydivides m1 m2 = m1 /= m2 && mdivides m1 m2


-- |We want to be able to construct monomials over any set of variables that we choose.
-- Although we will often use String as the type of our variables,
-- it is useful to define polymorphic types for monomials.
class MonomialConstructor m where
    mvar :: v -> m v
    mindices :: m v -> [(v,Int)]

-- |@var v@ creates a variable in the vector space of polynomials.
-- For example, if we want to work in Q[x,y,z], we might define:
--
-- > [x,y,z] = map var ["x","y","z"] :: [GlexPoly Q String]
--
-- Notice that, in general, it is necessary to provide a type annotation so that
-- the compiler knows which field and which term order to use.
var :: (Num k, MonomialConstructor m) => v -> Vect k (m v)
var = return . mvar

-- class MonomialOrder m where
--     isGraded :: m -> Bool


-- MONOMIALS

-- |The underlying implementation of monomials in variables of type v. Most often, we will be interested in MonImpl String,
-- with the variable \"x\" represented by M 1 [(\"x\",1)]. However, other types can be used instead.
--
-- No Ord instance is defined for MonImpl v, so it cannot be used as the basis for a free vector space of polynomials.
-- Instead, several different newtype wrappers are provided, corresponding to different monomial orderings.
data MonImpl v = M Int [(v,Int)] deriving (Eq, Functor)
-- The initial Int is the degree of the monomial. Storing it speeds up equality tests and comparisons

instance Show v => Show (MonImpl v) where
    show (M _ []) = "1"
    show (M _ xis) = concatMap (\(x,i) -> if i==1 then showVar x else showVar x ++ "^" ++ show i) xis
        where showVar x = filter ( /= '"' ) (show x) -- in case v == String

instance (Ord v) => Mon (MonImpl v) where
    munit = M 0 []
    mmult (M si xis) (M sj yjs) = M (si+sj) $ addmerge xis yjs

instance (Ord v, Show v) => Monomial (MonImpl v) where
    mdivides (M si xis) (M sj yjs) = si <= sj && mdivides' xis yjs where
        mdivides' ((x,i):xis) ((y,j):yjs) =
            case compare x y of
            LT -> False
            GT -> mdivides' ((x,i):xis) yjs
            EQ -> if i<=j then mdivides' xis yjs else False
        mdivides' [] _ = True
        mdivides' _ [] = False
    mdiv (M si xis) (M sj yjs) = M (si-sj) $ addmerge xis $ map (\(y,j) -> (y,-j)) yjs
    -- we don't check that the result has no negative indices
    mgcd (M _ xis) (M _ yjs) = mgcd' 0 [] xis yjs
        where mgcd' s zks ((x,i):xis) ((y,j):yjs) =
                  case compare x y of
                  LT -> mgcd' s zks xis ((y,j):yjs)
                  GT -> mgcd' s zks ((x,i):xis) yjs
                  EQ -> let k = min i j in mgcd' (s+k) ((x,k):zks) xis yjs
              mgcd' s zks _ _ = M s (reverse zks)
    mlcm (M si xis) (M sj yjs) = mlcm' 0 [] xis yjs
        where mlcm' s zks ((x,i):xis) ((y,j):yjs) =
                  case compare x y of
                  LT -> mlcm' (s+i) ((x,i):zks) xis ((y,j):yjs)
                  GT -> mlcm' (s+j) ((y,j):zks) ((x,i):xis) yjs
                  EQ -> let k = max i j in mlcm' (s+k) ((x,k):zks) xis yjs
              mlcm' s zks xis yjs = let zks' = xis ++ yjs; s' = sum (map snd zks') -- either xis or yjs is null
                                    in M (s+s') (reverse zks ++ zks')
    mcoprime (M _ xis) (M _ yjs) = mcoprime' xis yjs
        where mcoprime' ((x,i):xis) ((y,j):yjs) =
                  case compare x y of
                  LT -> mcoprime' xis ((y,j):yjs)
                  GT -> mcoprime' ((x,i):xis) yjs
                  EQ -> False
              mcoprime' _ _ = True
    -- mcoprime m1 m2 = mgcd m1 m2 == munit
    mdeg (M s _) = s

instance MonomialConstructor MonImpl where
    mvar v = M 1 [(v,1)]
    mindices (M si xis) = xis


-- LEX ORDER

-- |A type representing monomials with Lex ordering.
--
-- Lex stands for lexicographic ordering.
-- For example, in Lex ordering, monomials up to degree two would be ordered as follows: x^2+xy+xz+x+y^2+yz+y+z^2+z+1.
newtype Lex v = Lex (MonImpl v) deriving (Eq, Functor, Mon, Monomial, MonomialConstructor) -- GeneralizedNewtypeDeriving

instance Show v => Show (Lex v) where
    show (Lex m) = show m

instance Ord v => Ord (Lex v) where
    compare (Lex (M si xis)) (Lex (M sj yjs)) = compare' xis yjs
        where compare' ((x,i):xis) ((y,j):yjs) =
                  case compare x y of
                  LT -> LT
                  GT -> GT
                  EQ -> case compare i j of
                        LT -> GT
                        GT -> LT
                        EQ -> compare' xis yjs
              compare' [] [] = EQ
              compare' _ [] = LT
              compare' [] _ = GT
        -- unfortunately we can't use the following, because we want [] sorted after everything, not before
        -- compare [(x,-i) | (x,i) <- xis] [(y,-j) | (y,j) <- yjs]

-- instance MonomialOrder Lex where isGraded _ = False

-- |A type representing polynomials with Lex term ordering.
type LexPoly k v = Vect k (Lex v)

-- |@lexvar v@ creates a variable in the algebra of commutative polynomials over Q with Lex term ordering.
-- It is provided as a shortcut, to avoid having to provide a type annotation, as with @var@.
-- For example, the following code creates variables called x, y and z:
--
-- > [x,y,z] = map lexvar ["x","y","z"]
lexvar :: v -> LexPoly Q v
lexvar v = return $ Lex $ M 1 [(v,1)]
-- lexvar = var

instance (Eq k, Num k, Ord v, Show v) => Algebra k (Lex v) where
    unit x = x *> return munit
    mult xy = nf $ fmap (\(a,b) -> a `mmult` b) xy


-- GLEX ORDER

-- |A type representing monomials with Glex ordering.
--
-- Glex stands for graded lexicographic. Thus monomials are ordered first by degree, then by lexicographic order.
-- For example, in Glex ordering, monomials up to degree two would be ordered as follows: x^2+xy+xz+y^2+yz+z^2+x+y+z+1.
newtype Glex v = Glex (MonImpl v) deriving (Eq, Functor, Mon, Monomial, MonomialConstructor) -- GeneralizedNewtypeDeriving

instance Show v => Show (Glex v) where
    show (Glex m) = show m

instance Ord v => Ord (Glex v) where
    compare (Glex (M si xis)) (Glex (M sj yjs)) =
        compare (-si, [(x,-i) | (x,i) <- xis]) (-sj, [(y,-j) | (y,j) <- yjs])

-- instance MonomialOrder Glex where isGraded _ = True

-- |A type representing polynomials with Glex term ordering.
type GlexPoly k v = Vect k (Glex v)

-- |@glexvar v@ creates a variable in the algebra of commutative polynomials over Q with Glex term ordering.
-- It is provided as a shortcut, to avoid having to provide a type annotation, as with @var@.
-- For example, the following code creates variables called x, y and z:
--
-- > [x,y,z] = map glexvar ["x","y","z"]
glexvar :: v -> GlexPoly Q v
glexvar v = return $ Glex $ M 1 [(v,1)]
-- glexvar = var

instance (Eq k, Num k, Ord v, Show v) => Algebra k (Glex v) where
    unit x = x *> return munit
    mult xy = nf $ fmap (\(a,b) -> a `mmult` b) xy


-- GREVLEX ORDER

-- |A type representing monomials with Grevlex ordering.
--
-- Grevlex stands for graded reverse lexicographic. Thus monomials are ordered first by degree, then by reverse lexicographic order.
-- For example, in Grevlex ordering, monomials up to degree two would be ordered as follows: x^2+xy+y^2+xz+yz+z^2+x+y+z+1.
--
-- In general, Grevlex leads to the smallest Groebner bases.
newtype Grevlex v = Grevlex (MonImpl v) deriving (Eq, Functor, Mon, Monomial, MonomialConstructor) -- GeneralizedNewtypeDeriving

instance Show v => Show (Grevlex v) where
    show (Grevlex m) = show m

instance Ord v => Ord (Grevlex v) where
    compare (Grevlex (M si xis)) (Grevlex (M sj yjs)) =
        compare (-si, reverse xis) (-sj, reverse yjs)

-- instance MonomialOrder Grevlex where isGraded _ = True

-- |A type representing polynomials with Grevlex term ordering.
type GrevlexPoly k v = Vect k (Grevlex v)

-- |@grevlexvar v@ creates a variable in the algebra of commutative polynomials over Q with Grevlex term ordering.
-- It is provided as a shortcut, to avoid having to provide a type annotation, as with @var@.
-- For example, the following code creates variables called x, y and z:
--
-- > [x,y,z] = map grevlexvar ["x","y","z"]
grevlexvar :: v -> GrevlexPoly Q v
grevlexvar v = return $ Grevlex $ M 1 [(v,1)]
-- grevlexvar = var

instance (Eq k, Num k, Ord v, Show v) => Algebra k (Grevlex v) where
    unit x = x *> return munit
    mult xy = nf $ fmap (\(a,b) -> a `mmult` b) xy


-- ELIMINATION ORDER

data Elim2 a b = Elim2 !a !b deriving (Eq, Functor)

instance (Ord a, Ord b) => Ord (Elim2 a b) where
    compare (Elim2 a1 b1) (Elim2 a2 b2) = compare (a1,b1) (a2,b2)

instance (Show a, Show b) => Show (Elim2 a b) where
    show (Elim2 ma mb) = case (show ma, show mb) of
                       ("1","1") -> "1"
                       (ma',"1") -> ma'
                       ("1",mb') -> mb'
                       (ma',mb') -> ma' ++ mb'

instance (Mon a, Mon b) => Mon (Elim2 a b) where
    munit = Elim2 munit munit
    mmult (Elim2 a1 b1) (Elim2 a2 b2) = Elim2 (mmult a1 a2) (mmult b1 b2)

instance (Monomial a, Monomial b) => Monomial (Elim2 a b) where
    mdivides (Elim2 a1 b1) (Elim2 a2 b2) = mdivides a1 a2 && mdivides b1 b2
    mdiv (Elim2 a1 b1) (Elim2 a2 b2) = Elim2 (mdiv a1 a2) (mdiv b1 b2)
    mgcd (Elim2 a1 b1) (Elim2 a2 b2) = Elim2 (mgcd a1 a2) (mgcd b1 b2)
    mlcm (Elim2 a1 b1) (Elim2 a2 b2) = Elim2 (mlcm a1 a2) (mlcm b1 b2)
    mcoprime (Elim2 a1 b1) (Elim2 a2 b2) = mcoprime a1 a2 && mcoprime b1 b2
    mdeg (Elim2 a b) = mdeg a + mdeg b

instance (Eq k, Num k, Ord a, Mon a, Ord b, Mon b) => Algebra k (Elim2 a b) where
    unit x = x *> return munit
    mult xy = nf $ fmap (\(a,b) -> a `mmult` b) xy


-- VARIABLE SUBSTITUTION

-- |Given (Num k, MonomialConstructor m), then Vect k (m v) is the free commutative algebra over v.
-- As such, it is a monad (in the mathematical sense). The following pseudo-code (not legal Haskell)
-- shows how this would work:
--
-- > instance (Num k, Monomial m) => Monad (\v -> Vect k (m v)) where
-- >     return = var
-- >     (>>=) = bind
--
-- bind corresponds to variable substitution, so @v \`bind\` f@ returns the result of making the substitutions
-- encoded in f into v.
--
-- Note that the type signature is slightly more general than that required by (>>=).
-- For a monad, we would only require:
--
-- > bind :: (MonomialConstructor m, Num k, Ord (m v), Show (m v), Algebra k (m v)) =>
-- >     Vect k (m u) -> (u -> Vect k (m v)) -> Vect k (m v)
--
-- Instead, the given type signature allows us to substitute in elements of any algebra.
-- This is occasionally useful.

-- |bind performs variable substitution
bind :: (Eq k, Num k, MonomialConstructor m, Ord a, Show a, Algebra k a) =>
    Vect k (m v) -> (v -> Vect k a) -> Vect k a
v `bind` f = linear (\m -> product [f x ^ i | (x,i) <- mindices m]) v
-- V ts `bind` f = sum [c *> product [f x ^ i | (x,i) <- mindices m] | (m, c) <- ts] 

-- We can't express the Monad instance directly in Haskell, firstly because of the Ord v constraint (? - not used),
-- secondly because Haskell doesn't support type functions.

flipbind f = linear (\m -> product [f x ^ i | (x,i) <- mindices m])

-- |Evaluate a polynomial at a point.
-- For example @eval (x^2+y^2) [(x,1),(y,2)]@ evaluates x^2+y^2 at the point (x,y)=(1,2).
eval :: (Eq k, Num k, MonomialConstructor m, Eq (m v), Show v) =>
    Vect k (m v) -> [(Vect k (m v), k)] -> k
eval f vs = unwrap $ f `bind` sub
    where sub x = case lookup (var x) vs of
                  Just xval -> xval *> return ()
                  Nothing -> error ("eval: no binding given for " ++ show x)

-- |Perform variable substitution on a polynomial.
-- For example @subst (x*z-y^2) [(x,u^2),(y,u*v),(z,v^2)]@ performs the substitution x -> u^2, y -> u*v, z -> v^2.
subst :: (Eq k, Num k, MonomialConstructor m, Eq (m u), Show u, Ord (m v), Show (m v), Algebra k (m v)) =>
    Vect k (m u) -> [(Vect k (m u), Vect k (m v))] -> Vect k (m v)
subst f vs = f `bind` sub
    where sub x = case lookup (var x) vs of
                  Just xsub -> xsub
                  Nothing -> error ("eval: no binding given for " ++ show x)
-- The type could be more general than this, but haven't so far found a use case

-- |List the variables used in a polynomial
vars :: (Num k, Ord k, MonomialConstructor m, Ord (m v)) =>
     Vect k (m v) -> [Vect k (m v)]
vars f = toSet [ var v | (m,_) <- terms f, v <- map fst (mindices m) ]


-- DIVISION ALGORITHM FOR POLYNOMIALS

lt (V (t:ts)) = t -- leading term
lm = fst . lt     -- leading monomial
lc = snd . lt     -- leading coefficient

-- deg :: (Num k, Monomial m, MonomialOrder m) => Vect k m -> Int
deg (V []) = -1
deg f = maximum $ [mdeg m | (m,c) <- terms f]
{-
deg f | isGraded (lm f) = mdeg (lm f)
      | otherwise = maximum $ [mdeg m | (m,c) <- terms f]
-}
-- the true degree of the polynomial, not the degree of the leading term
-- required for sugar strategy when computing Groebner basis

toMonic 0 = 0
toMonic f = (1 / lc f) *> f

-- tdivmaybe (m1,x1) (m2,x2) = fmap (\m -> (m,x1/x2)) $ mdivmaybe m1 m2

tdivides (m1,x1) (m2,x2) = mdivides m1 m2

tdiv (m1,x1) (m2,x2) = (mdiv m1 m2, x1/x2)

tgcd (m1,_) (m2,_) = (mgcd m1 m2, 1)
-- tlcm (m1,_) (m2,_) = (mlcm m1 m2, 1)

tmult (m,c) (m',c') = (mmult m m',c*c')

infixl 7 *->
t *-> V ts = V $ map (tmult t) ts -- preserves term order


-- given f, gs, find as, r such that f = sum (zipWith (*) as gs) + r, with r not divisible by any g
quotRemMP f gs = quotRemMP' f (replicate n 0, 0) where
    n = length gs
    quotRemMP' 0 (us,r) = (us,r)
    quotRemMP' h (us,r) = divisionStep h (gs,[],us,r)
    divisionStep h (g:gs,us',u:us,r) =
        if lt g `tdivides` lt h
        then let t = V [lt h `tdiv` lt g]
                 h' = h - t*g
                 u' = u+t
             in quotRemMP' h' (reverse us' ++ u':us, r)
        else divisionStep h (gs,u:us',us,r)
    divisionStep h ([],us',[],r) =
        let (lth,h') = splitlt h
        in quotRemMP' h' (reverse us', r+lth)
    splitlt (V (t:ts)) = (V [t], V ts)


rewrite f gs = rewrite' (f,0) gs where
    rewrite' (0,r) _ = r
    rewrite' (l,r) (h:hs) =
        if lt h `tdivides` lt l -- if lhs of "rewrite rule" h matches
        then let l' = l - V [lt l `tdiv` lt h] * h -- apply rewrite rule to eliminate leading term
             in rewrite' (l',r) gs -- then start again and try to eliminate the new lt.
        else rewrite' (l,r) hs -- else try the next potential divisor
    rewrite' (l,r) [] = -- none of the rewrite rules matches lt l
        let (h,t) = split l
        in rewrite' (t, r + h) gs -- so move it into the remainder r, and try to rewrite the other terms
    split (V (t:ts)) = (V [t], V ts)


infixl 7 %%

-- |@f %% gs@ is the reduction of a polynomial f with respect to a list of polynomials gs.
-- In the case where the gs are a Groebner basis for an ideal I,
-- then @f %% gs@ is the equivalence class representative of f in R/I,
-- and is zero if and only if f is in I.
(%%) :: (Eq k, Fractional k, Monomial m, Ord m, Algebra k m) =>
     Vect k m -> [Vect k m] -> Vect k m
f %% gs = rewrite f gs
-- f %% gs = r where (_,r) = quotRemMP f gs


-- |As a convenience, a partial instance of Fractional is defined for polynomials.
-- The instance is well-defined only for scalars, and gives an error if used on other values.
-- The purpose of this is to allow entry of fractional scalars, in expressions such as @x/2@.
-- On the other hand, an expression such as @2/x@ will return an error.
instance (Eq k, Fractional k, Monomial m, Ord m, Algebra k m) => Fractional (Vect k m) where
    recip (V [(m,c)]) | m == munit = V [(m,1/c)]
                      | otherwise = error "Polynomial recip: only defined for scalars"
    fromRational x = V [(munit, fromRational x)]
