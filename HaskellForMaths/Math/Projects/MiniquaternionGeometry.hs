-- Copyright (c) David Amos, 2009. All rights reserved.

module Math.Projects.MiniquaternionGeometry where

import qualified Data.List as L

import Math.Common.ListSet as LS
import Math.Core.Utils (combinationsOf)

import Math.Algebra.Field.Base
import Math.Combinatorics.FiniteGeometry (pnf, ispnf, orderPGL)
-- import Math.Combinatorics.Graph
import Math.Combinatorics.GraphAuts
import Math.Algebra.Group.PermutationGroup hiding (order)
import qualified Math.Algebra.Group.SchreierSims as SS
import Math.Algebra.Group.RandomSchreierSims
import Math.Combinatorics.Design as D
import Math.Algebra.LinearAlgebra -- ( (<.>), (<+>) )

import Math.Projects.ChevalleyGroup.Classical


-- Sources:
-- Miniquaternion Geometry, Room & Kirkpatrick
-- Survey of Non-Desarguesian Planes, Charles Weibel


-- F9, defined by adding sqrt of -1 to F3. (The Conway poly for F9 is not so convenient for us here)
data F9 = F9 F3 F3 deriving (Eq,Ord)

instance Show F9 where
    show (F9 0 0) = "0"
    show (F9 0 1) = "e"
    show (F9 0 2) = "-e"
    show (F9 1 0) = "1"
    show (F9 1 1) = "1+e"
    show (F9 1 2) = "1-e"
    show (F9 2 0) = "-1"
    show (F9 2 1) = "-1+e"
    show (F9 2 2) = "-1-e"

e = F9 0 1 -- sqrt of -1

instance Num F9 where
    F9 a1 b1 + F9 a2 b2 = F9 (a1+a2) (b1+b2)
    F9 a1 b1 * F9 a2 b2 = F9 (a1*a2-b1*b2) (a1*b2+a2*b1)
    negate (F9 a b) = F9 (negate a) (negate b)
    fromInteger n = F9 (fromInteger n) 0

f9 = [F9 a b | a <- f3, b <- f3]

w = 1-e -- a primitive element - generates the multiplicative group

conj (F9 a b) = F9 a (-b)
-- This is just the Frobenius aut x -> x^3

norm (F9 a b) = a^2 + b^2
-- == x * conj x

instance Fractional F9 where
    recip x@(F9 a b) = F9 (a/n) (-b/n) where n = norm x

instance FiniteField F9 where
    basisFq _ = [1,e]


-- J9, or Q, defined by modifying the multiplication in F9
data J9 = J9 F9 deriving (Eq,Ord)

instance Show J9 where
    show (J9 (F9 0 0)) = "0"
    show (J9 (F9 0 1)) = "-j"
    show (J9 (F9 0 2)) = "j"
    show (J9 (F9 1 0)) = "1"
    show (J9 (F9 1 1)) = "-k"
    show (J9 (F9 1 2)) = "i"
    show (J9 (F9 2 0)) = "-1"
    show (J9 (F9 2 1)) = "-i"
    show (J9 (F9 2 2)) = "k"

squaresF9 = [1,w^2,w^4,w^6] -- and 0, but not needed here

instance Num J9 where
    J9 x + J9 y = J9 (x+y)
    0 * _ = 0
    _ * 0 = 0
    J9 x * J9 y =
        if y `elem` squaresF9
        then J9 (x*y)
        else J9 (conj x * y)
    negate (J9 x) = J9 (negate x)
    fromInteger n = J9 (fromInteger n)

i = J9 w
j = J9 (w^6) -- == i-1
k = J9 (w^7) -- == i+1

j9 = [J9 x | x <- f9]


-- the aut of J9 that sends i to x
autJ9 x = fromPairs [ (a+b*i, a+b*x) | a <- [0,1,-1], b <- [1,-1] ]

autA = autJ9 (-i) -- sends i -> -i
autB = autJ9 (-k) -- sends j -> -j
autC = autJ9 (-j) -- sends k -> -k

autsJ9 = [autA, autC]
-- these two auts generate the group, which is isomorphic to S3
-- indeed, the auts permute the pairs {i,-i}, {j,-j}, {k,-k}


conj' (J9 x) = J9 (conj x)
-- Note that conj' x == x .^ autB


isAut k sigma = and [sigma x + sigma y == sigma (x+y) | x <- k, y <- k]
             && and [sigma x * sigma y == sigma (x*y) | x <- k, y <- k]


isReal x = x `elem` [0,1,-1]
isComplex = not . isReal

instance Fractional J9 where
    recip 0 = error "J9.recip: 0"
    recip x | isReal x  = x
            | otherwise = -x

instance FiniteField J9 where
    basisFq _ = [1,i,j,k]
    eltsFq _ = j9


-- PROJECTIVE PLANES

ptsPG2 r =  [ [0,0,1] ] ++ [ [0,1,x] | x <- r ] ++ [ [1,x,y] | x <- r, y <- r ]
-- if r is sorted, then so is the result

orthogonalLinesPG2 xs = L.sort [ [x | x <- xs, u <.> x == 0] | u <- xs ]

rightLinesPG2 r =
    [ [0,0,1] : [ [0,1,x] | x <- r] ] ++ -- line at infinity
    [ [0,0,1] : [ [1,x,y] | y <- r] | x <- r ] ++ -- vertical lines
    [ [0,1,a] : [ [1,x,y] | x <- r, y <- [x*a+b] ] | a <- r, b <- r ] -- slope multiplies on the right
-- if r is sorted, then so is the result, and each line in the result

leftLinesPG2 r =
    [ [0,0,1] : [ [0,1,x] | x <- r] ] ++ -- line at infinity
    [ [0,0,1] : [ [1,x,y] | y <- r] | x <- r ] ++ -- vertical lines
    [ [0,1,a] : [ [1,x,y] | x <- r, y <- [a*x+b] ] | a <- r, b <- r ] -- slope multiplies on the left


-- Projective plane PG2(F9)
phi = design (xs,bs) where
    xs = ptsPG2 f9
    bs = orthogonalLinesPG2 xs -- L.sort [ [x | x <- xs, u <.> x == 0] | u <- xs ]

-- Then the collineations of phi consist of projective transformations,
-- together with a conjugacy collineation induced by the Frobenius aut

-- alternative construction of PG2(F9) - gives same result
phi' = design (xs,bs) where
    xs = ptsPG2 f9
    bs = rightLinesPG2 f9


collineationsPhi = l 3 f9 ++ [fieldAut] where
    D xs bs = phi
    fieldAut = fromPairs [ (x , map conj x) | x <- xs ]
-- in general, this would be PSigmaL(n,Fq), whereas we want PGammaL(n,Fq). However, for F9 they coincide.
-- order 84913920


liftToGraph (D xs bs) g = fromPairs $ [(Left x, Left (x .^ g)) | x <- xs] ++ [(Right b, Right (b -^ g)) | b <- bs]



-- This construction appears to produce a projective plane
-- (However, Room & Kirkpatrick point out that it's not really well-defined
-- - if we had chosen different quasi-homogeneous coords, we would have got different results)
-- However, it's not the same as either omega or omegaD below
omega0 = design (xs,bs) where
    xs = ptsPG2 j9
    bs = orthogonalLinesPG2 xs -- L.sort [ [x | x <- xs, u <.> x == 0] | u <- xs ]


-- Room & Kirkpatrick, p103
omega = design (xs,bs) where
    xs = ptsPG2 j9
    bs = rightLinesPG2 j9

-- another construction that produces same result (but slower)
omega2 = design (xs,bs) where
    xs = ptsPG2 j9
    bs =  [ l | [p,q] <- combinationsOf 2 xs, l <- [line p q], [p,q] == take 2 l]
    line p q = toListSet $ filter ispnf [(a *> p) <+> (b *> q) | a <- j9, b <- j9]


-- Room & Kirkpatrick, p107, p114
collineationsOmega =
    [r]
 ++ [s rho sigma | rho <- j9 \\ [0], sigma <- j9 \\ [0], rho == 1 || sigma == 1]
 ++ [t delta epsilon | delta <- j9, epsilon <- j9, delta * epsilon == 0] -- for generators sufficient to have only one non-zero
 ++ [u]
 ++ [a lambda | lambda <- autsJ9] where
    D xs bs = omega
    fromMatrix m = fromPairs [ (x, pnf (x <*>> m)) | x <- xs]
    r = fromMatrix [[1,0,0],[0,0,1],[0,1,0]] -- reflect in the line x = y in the affine subplane
    s rho sigma = fromPairs $ [([1,x,y], [1,x*rho,y*sigma]) | x <- j9, y <- j9]
                           ++ [([0,1,mu],[0,1,(recip rho)*mu*sigma]) | mu <- j9]
                           ++ [([0,0,1],[0,0,1])] -- leaves "Y" fixed
    -- fromMatrix [[1,0,0],[0,rho,0],[0,0,sigma]] -- scale x,y -> rho x, sigma y
    t delta epsilon = fromMatrix [[1,delta,epsilon],[0,1,0],[0,0,1]] -- translation x,y -> x+delta, y+epsilon
    u = fromPairs $ [([1,x,y], [1,x+y,x-y]) | x <- j9, y <- j9]
                           ++ [([0,1,mu],[0,1,-mu]) | mu <- filter isComplex j9]
                           ++ [([0,1,0],[0,1,1]), ([0,1,1],[0,1,0]), ([0,1,-1],[0,0,1]), ([0,0,1],[0,1,-1])]
    -- fromMatrix [[1,0,0],[0,1,-1],[0,1,1]]
    a lambda = fromPairs [ (x, map (.^ lambda) x) | x <- xs]
-- order 311040
-- (which means this is also the plane constructed in Weibel?)


-- dual plane of omega
omegaD = design (xs,bs) where
    xs = ptsPG2 j9
    bs = leftLinesPG2 j9

omegaD1 = D.to1n $ dual omega
-- need proof omega /~= omegaD

omegaD2 = design (xs,bs) where
    xs = ptsPG2 j9
    bs =  [ l | [p,q] <- combinationsOf 2 xs, l <- [line p q], [p,q] == take 2 l]
    line p q = toListSet $ filter ispnf [(p <* a) <+> (q <* b) | a <- j9, b <- j9]

us <* x = map (*x) us


-- Room and Kirkpatrick p130
psi = design (xs,bs) where
    xs = ptsPG2 j9
    isReal x = all (`elem` [0,1,-1]) x
    xrs = ptsPG2 [0,1,-1] -- the thirteen real points, a copy of PG2(F3) within psi
    bs = toListSet [line p q | p <- xrs, q <- xs, q /= p]
    line p q = L.sort $ p : [pnf ( (p <* a) <+> q) | a <- j9]


-- Room & Kirkpatrick p137
psi2 = design (xs,bs) where
    xs = ptsPG2 j9
    bs = L.sort $
         [ [0,0,1] : [ [0,1,x] | x <- j9] ] ++ -- line at infinity, z=0
         [ [0,0,1] : [ [1,kappa,y] | y <- j9] | kappa <- j9 ] ++ -- vertical lines x = kappa
         [ [0,1,m] : [ [1,x,m*x+kappa] | x <- j9 ] | m <- [0,1,-1], kappa <- j9 ] ++ -- lines with real slope
         [ [0,1,kappa] : [ [1,x,kappa*(x-r)+s] | x <- j9 ] | r <- [0,1,-1], s <- [0,1,-1], kappa <- j9 \\ [0,1,-1] ]
         -- lines with complex slope

-- Room & Kirkpatrick p134-6
collineationsPsi = realProjectivities -- real transvections, generating real projectivities
                ++ [a lambda | lambda <- autsJ9] where
    D xs bs = psi
    n = 3
    realTransvections = [elemTransvection n (r,c) l | r <- [1..n], c <- [1..n], r /= c, l <- [1]]
    realProjectivities = [fromPairs $ [(x, pnf (x <*>> m)) | x <- xs] | m <- realTransvections]
    a lambda = fromPairs [ (x, map (.^ lambda) x) | x <- xs]
-- order 33696


-- The order of a projective plane
order (D xs bs) = length (head bs) - 1

isProjectivePlane pi = designParams pi == Just (2,(q^2+q+1,q,1))
    where q = order pi


collinear (D xs bs) ys = (not . null) [b | b <- bs, all (`elem` b) ys]

-- assume p1..4 are distinct
isQuadrangle plane ps@[p1,p2,p3,p4] =
    all (not . collinear plane) (combinationsOf 3 ps)


concurrent (D xs bs) ls = (not . null) [x | x <- xs, all (x `elem`) ls]

isQuadrilateral plane ls@[l1,l2,l3,l4] =
    all (not . concurrent plane) (combinationsOf 3 ls)


isOval pi ps = length ps == order pi+1
            && all (not . collinear pi) (combinationsOf 3 ps)

findOvals1 pi = findOvals' 0 ([], points pi) where
    n = order pi
    findOvals' i (ls,rs)
        | i == n+1 = [reverse ls]
        | otherwise = concatMap (findOvals' (i+1))
                      [ (r:ls, rs') | r:rs' <- L.tails rs, all (not . collinear pi) (map (r:) (combinationsOf 2 ls)) ]
-- if we have a function to quickly generate the line through two points,
-- then we just need to see whether the third point is on it, which is much faster than testing collinearity

findQuadrangles pi = findQuadrangles' 0 ([], points pi) where
    findQuadrangles' i (ls,rs)
        | i == 4 = [reverse ls]
        | otherwise = concatMap (findQuadrangles' (i+1))
                      [ (r:ls, rs') | r:rs' <- L.tails rs, all (not . collinear pi) (map (r:) (combinationsOf 2 ls)) ]


findOvals pi@(D xs bs) = findOvals' 0 ([],xs) bs where
    n = order pi
    findOvals' i (ls,rs) bs
        | i == n+1 = [reverse ls]
        | otherwise = concat
                      [let rls = reverse (r:ls)
                           (notchords, chords) = L.partition (\b -> length (rls `LS.intersect` b) < 2) bs
                           rs'' = foldl (\\) rs' chords
                           -- if any line is already a chord, remove remaining points on it from further consideration
                       in findOvals' (i+1) (r:ls, rs'') notchords
                       | r:rs' <- L.tails rs]

-- Todo:
-- Code that shows that phi is Desarguesian, and omega, omegaD and psi are not
{-
-- !! NOT WORKING
-- finds apparent counterexamples in phi too
findNonDesarguesian pi@(D xs bs) =
    [ [p,x,y,z,x',y',z',k,l,m] | p <- xs,
                                 x <- xs \\ [p],
                                 y <- xs \\ [p,x],
                                 z <- xs \\ [p,x,y],
                                 (not . collinear pi) [x,y,z],
                                 x' <- line p x \\ L.sort [p,x],
                                 y' <- line p y \\ L.sort [p,y],
                                 z' <- line p z \\ L.sort [p,z],
                                 (not . collinear pi) [x',y',z'],
                                 k <- line x y `intersect` line x' y', -- will only have one element
                                 l <- line x z `intersect` line x' z',
                                 m <- line y z `intersect` line y' z',
                                 (not . collinear pi) [k,l,m]  ]
    where line p q = head [b | b <- bs, p `elem` b, q `elem` b]
-}