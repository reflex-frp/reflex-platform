-- Copyright (c) 2010, David Amos. All rights reserved.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |A module defining the algebra of commutative polynomials over a field k
module Math.Algebras.Commutative where

import Math.Algebra.Field.Base hiding (powers)
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Algebras.Structures


-- GLEX MONOMIALS

data GlexMonomial v = Glex Int [(v,Int)] deriving (Eq)
-- The initial Int is the degree of the monomial. Storing it speeds up equality tests and comparisons

-- type GlexMonomialS = GlexMonomial String

instance Ord v => Ord (GlexMonomial v) where
    compare (Glex si xis) (Glex sj yjs) = compare (-si, [(x,-i) | (x,i) <- xis]) (-sj, [(y,-j) | (y,j) <- yjs])

instance Show v => Show (GlexMonomial v) where
    show (Glex _ []) = "1"
    show (Glex _ xis) = concatMap (\(x,i) -> if i==1 then showVar x else showVar x ++ "^" ++ show i) xis
        where showVar x = filter ( /= '"' ) (show x) -- in case v == String

{-
-- GlexMonomial is a functor and a monad
-- However, this isn't all that much use, and to make proper use of it we'd need a "nf" function
-- So leaving this commented out

-- map one basis to another
instance Functor GlexMonomial where
    fmap f (Glex si xis) = Glex si [(f x, i) | (x,i) <- xis]
-- Note that as we can't assume the Ord instance, we would need to call "nf" afterwards

-- GlexMonomial is the free commutative monoid, and hence a monad
instance Monad GlexMonomial where
    return x = Glex 1 [(x,1)]
    (Glex _ xis) >>= f = let parts = [(i, sj, yjs) | (x,i) <- xis, let Glex sj yjs = f x]
                         in Glex (sum [i*sj | (i,sj,_) <- parts])
                                 (concatMap (\(i,_,yjs)->map (\(y,j)->(y,i*j)) yjs) parts)
    -- this isn't really much use - it's variable substitution, but we're only allowed to substitute monomials for each var
-- Note that as we can't assume the Ord instance, we would need to call "nf" afterwards
-}

-- This is the monoid algebra for commutative monomials (which are the free commutative monoid)
instance (Eq k, Num k, Ord v) => Algebra k (GlexMonomial v) where
    unit x = x *> return munit
        where munit = Glex 0 []
    mult xy = nf $ fmap (\(a,b) -> a `mmult` b) xy
        where mmult (Glex si xis) (Glex sj yjs) = Glex (si+sj) $ addmerge xis yjs


-- GlexPoly can be given the set coalgebra structure, which is compatible with the monoid algebra structure
instance (Eq k, Num k) => Coalgebra k (GlexMonomial v) where
    counit = unwrap . nf . fmap (\m -> () )  -- trace
    -- counit (V ts) = sum [x | (m,x) <- ts]  -- trace
    comult = fmap (\m -> (m,m) )             -- diagonal

type GlexPoly k v = Vect k (GlexMonomial v)


-- |glexVar creates a variable in the algebra of commutative polynomials with Glex term ordering.
-- For example, the following code creates variables called x, y and z:
--
-- > [x,y,z] = map glexVar ["x","y","z"] :: GlexPoly Q String
glexVar :: (Num k) => v -> GlexPoly k v
glexVar v = V [(Glex 1 [(v,1)], 1)]


class Monomial m where
    var :: v -> Vect Q (m v)
    powers :: m v -> [(v,Int)]

-- |In effect, we have (Num k, Monomial m) => Monad (\v -> Vect k (m v)), with return = var, and (>>=) = bind.
-- However, we can't express this directly in Haskell, firstly because of the Ord b constraint,
-- secondly because Haskell doesn't support type functions.
bind :: (Monomial m, Eq k, Num k, Ord b, Show b, Algebra k b) =>
     Vect k (m v) -> (v -> Vect k b) -> Vect k b
V ts `bind` f = sum [c *> product [f x ^ i | (x,i) <- powers m] | (m, c) <- ts] 
-- flipbind f = linear (\m -> product [f x ^ i | (x,i) <- powers m])


instance Monomial GlexMonomial where
    var = glexVar
    powers (Glex _ xis) = xis


-- DIVISION

lt (V (t:ts)) = t

class DivisionBasis b where
    dividesB :: b -> b -> Bool
    divB :: b -> b -> b

dividesT (b1,x1) (b2,x2) = dividesB b1 b2
divT (b1,x1) (b2,x2) = (divB b1 b2, x1/x2)

-- given f, gs, find as, r such that f = sum (zipWith (*) as gs) + r, with r not divisible by any g
quotRemMP f gs = quotRemMP' f (replicate n 0, 0) where
    n = length gs
    quotRemMP' 0 (us,r) = (us,r)
    quotRemMP' h (us,r) = divisionStep h (gs,[],us,r)
    divisionStep h (g:gs,us',u:us,r) =
        if lt g `dividesT` lt h
        then let t = V [lt h `divT` lt g]
                 h' = h - t*g
                 u' = u+t
             in quotRemMP' h' (reverse us' ++ u':us, r)
        else divisionStep h (gs,u:us',us,r)
    divisionStep h ([],us',[],r) =
        let (lth,h') = splitlt h
        in quotRemMP' h' (reverse us', r+lth)
    splitlt (V (t:ts)) = (V [t], V ts)

infixl 7 %%

-- |(%%) reduces a polynomial with respect to a list of polynomials.
(%%) :: (Eq k, Fractional k, Ord b, Show b, Algebra k b, DivisionBasis b)
     => Vect k b -> [Vect k b] -> Vect k b
f %% gs = r where (_,r) = quotRemMP f gs


instance Ord v => DivisionBasis (GlexMonomial v) where
    dividesB (Glex si xis) (Glex sj yjs) = si <= sj && dividesB' xis yjs where
        dividesB' ((x,i):xis) ((y,j):yjs) =
            case compare x y of
            LT -> False
            GT -> dividesB' ((x,i):xis) yjs
            EQ -> if i<=j then dividesB' xis yjs else False
        dividesB' [] _ = True
        dividesB' _ [] = False
    divB (Glex si xis) (Glex sj yjs) = Glex (si-sj) $ divB' xis yjs where
        divB' ((x,i):xis) ((y,j):yjs) =
            case compare x y of
            LT -> (x,i) : divB' xis ((y,j):yjs)
            EQ -> if i == j then divB' xis yjs else (x,i-j) : divB' xis yjs -- we don't bother to check i > j
            GT -> error "divB'" -- (y,-j) : divB' ((x,i):xis) yjs
        divB' xis [] = xis
        divB' [] yjs = error "divB'"

{-
-- Need to thread this through Maybe properly, so perhaps use do notation
divB2 (Glex si xis) (Glex sj yjs)
    | si < sj = Nothing
    | otherwise = case divB' xis yjs of
                  Nothing -> Nothing
                  Just zks -> Glex (si-sj) zks
    where divB' ((x,i):xis) ((y,j):yjs) =
              case compare x y of
              LT -> (x,i) : divB' xis ((y,j):yjs)
              EQ -> case compare i j of
                    LT -> Nothing
                    EQ -> divB' xis yjs
                    GT -> (x,i-j) : divB' xis yjs
              GT -> Nothing
-}
-- !! could change divB to return Maybe, and avoid need for dividesB


