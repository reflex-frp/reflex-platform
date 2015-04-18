-- Copyright (c) 2006-2011, David Amos. All rights reserved.

-- |A module for finding prime factors.
module Math.NumberTheory.Factor (module Math.NumberTheory.Prime,
                                 pfactors) where

import Math.NumberTheory.Prime
import Data.Either (lefts)
import Data.List (zip4)

-- Cohen, A Course in Computational Algebraic Number Theory, p488


-- return (u,v,d) s.t ua+vb = d, with d = gcd a b
extendedEuclid a b
    | b == 0 = (1,0,a)
    | otherwise = let (q,r) = a `quotRem` b        -- a == qb + r
                      (s,t,d) = extendedEuclid b r -- s*b+t*r == d
                  in (t,s-q*t,d)                   -- s*b+t*(a-q*b) == d


-- ELLIPTIC CURVE ARITHMETIC

data EllipticCurve = EC Integer Integer Integer deriving (Eq, Show)
-- EC p a b represents the curve y^2 == x^3+ax+b over Fp

data EllipticCurvePt = Inf | P Integer Integer deriving (Eq, Show)
-- P x y

isEltEC _ Inf = True
isEltEC (EC n a b) (P x y) = (y*y - x*x*x - a*x - b) `mod` n == 0


-- Koblitz p34

-- Addition in an elliptic curve, with bailout if the arithmetic fails (giving a potential factor of n)
ecAdd _ Inf pt = Right pt
ecAdd _ pt Inf = Right pt
ecAdd (EC n a b) (P x1 y1) (P x2 y2)
    | x1 /= x2 = let (_,v,d) = extendedEuclid n ((x1-x2) `mod` n)  -- we're expecting d == 1, v == 1/(x1-x2) (mod n)
                     m = (y1-y2) * v `mod` n
                     x3 = (m*m - x1 - x2) `mod` n
                     y3 = (-y1 + m*(x1 - x3)) `mod` n
                  in if d == 1 then Right (P x3 y3) else Left d
    | x1 == x2 = if (y1 + y2) `mod` n == 0  -- includes the case y1 == y2 == 0
                 then Right Inf
                 else let (_,v,d) = extendedEuclid n ((2*y1) `mod` n)  -- we're expecting d == 1, v == 1/(2*y1) (mod n)
                          m = (3*x1*x1 + a) * v `mod` n
                          x3 = (m*m - 2*x1) `mod` n
                          y3 = (-y1 + m*(x1 - x3)) `mod` n
                      in if d == 1 then Right (P x3 y3) else Left d
-- Note that b isn't actually used anywhere

-- Note, only the final `mod` n calls when calculating x3, y3 are necessary
-- and the code is faster if the others are removed

-- Scalar multiplication in an elliptic curve
ecSmult _ 0 _ = Right Inf
ecSmult ec k pt | k > 0 = ecSmult' k pt Inf
    where -- ecSmult' k q p = k * q + p
          ecSmult' 0 _ p = Right p
          ecSmult' i q p = let p' = if odd i then ecAdd ec p q else Right p
                               q' = ecAdd ec q q
                           in case (p',q') of
                              (Right p'', Right q'') -> ecSmult' (i `div` 2) q'' p''
                              (Left _, _) -> p'
                              (_, Left _) -> q'


-- ELLIPTIC CURVE FACTORISATION

-- We choose an elliptic curve E over Zn, and a point P on the curve
-- We then try to calculate kP, for suitably chosen k
-- What we are hoping is that at some stage we will fail because we can't invert an element in Zn
-- This will lead to finding a non-trivial factor of n


discriminantEC a b = 4 * a * a * a + 27 * b * b

-- perform a sequence of scalar multiplications in the elliptic curve, hoping for a bailout
ecTrial ec@(EC n a b) ms pt
    | d == 1 = ecTrial' ms pt
    | otherwise = Left d
    where d = gcd n (discriminantEC a b)
          ecTrial' [] pt = Right pt
          ecTrial' (m:ms) pt = case ecSmult ec m pt of
                               Right pt' -> ecTrial' ms pt'
                               Left d -> Left d
-- In effect, we're calculating ecSmult ec (product ms) pt, but an m at a time

l n = exp (sqrt (log n * log (log n)))
-- L(n) is some sort of measure of the average smoothness of numbers up to n
-- # [x <= n | x is L(n)^a-smooth] = n L(n)^(-1/2a+o(1))  -- Cohen p 482

-- q is the largest prime we're looking for - normally sqrt n
-- the b figure here is from Cohen p488
multipliers q = [p' | p <- takeWhile (<= b) primes, let p' = last (takeWhile (<= b) (powers p))]
    where b = round ((l q) ** (1/sqrt 2))
          powers x = iterate (*x) x

findFactorECM n | gcd n 6 == 1 =
    let ms = multipliers (sqrt $ fromInteger n)
    in head $ filter ( (/= 0) . (`mod` n) ) $
       lefts [ecTrial (EC n a 1) ms (P 0 1) | a <- [1..] ]
    -- the filter is because d might be a multiple of n,
    -- for example if the problem was that the discriminant was divisible by n


-- |List the prime factors of n (with multiplicity).
-- The algorithm uses trial division to find small factors,
-- followed if necessary by the elliptic curve method to find larger factors.
-- The running time increases with the size of the second largest prime factor of n.
-- It can find 10-digit prime factors in seconds, but can struggle with 20-digit prime factors.
pfactors :: Integer -> [Integer]
pfactors n | n > 0 = pfactors' n $ takeWhile (< 10000) primes
           | n < 0 = -1 : pfactors' (-n) (takeWhile (< 10000) primes)
    where pfactors' n (d:ds) | n == 1 = []
                             | n < d*d = [n]
                             | r == 0 = d : pfactors' q (d:ds)
                             | otherwise = pfactors' n ds
                             where (q,r) = quotRem n d
          pfactors' n [] = pfactors'' n
          pfactors'' n = if isMillerRabinPrime n then [n]
                         else let d = findFactorParallelECM n -- findFactorECM n
                              in merge (pfactors'' d) (pfactors'' (n `div` d))

merge (x:xs) (y:ys) =
    case compare x y of
    LT -> x : merge xs (y:ys)
    EQ -> x : y : merge xs ys
    GT -> y : merge (x:xs) ys
merge xs ys = xs ++ ys


-- Cohen p489
-- find inverse of as mod n in parallel, or a non-trivial factor of n
parallelInverse n as = if d == 1 then Right bs else Left $ head [d' | a <- as, let d' = gcd a n, d' /= 1]
    where c:cs = reverse $ scanl (\x y -> x*y `mod` n) 1 as
          ds = scanl (\x y -> x*y `mod` n) 1 (reverse as)
          (u,_,d) = extendedEuclid c n
          bs = reverse [ u*nota `mod` n | nota <- zipWith (*) cs ds]
-- let m = length as
-- then the above code requires O(m) mod calls - in fact 3m-3 calls (?)

parallelEcAdd n ecs ps1 ps2 =
    case parallelInverse n (zipWith f ps1 ps2) of
    Right invs -> Right [g ec p1 p2 inv | (ec,p1,p2,inv) <- zip4 ecs ps1 ps2 invs]
    Left d -> Left d
    where f Inf pt = 1
          f pt Inf = 1
          f (P x1 y1) (P x2 y2) | x1 /= x2 = x1-x2 -- slightly faster not to `mod` n here
                                | x1 == x2 = 2*y1 -- slightly faster not to `mod` n here
          -- inverses = parallelInverse n $ zipWith f ps1 ps2
          g _ Inf pt _ = pt
          g _ pt Inf _ = pt
          g (EC n a b) (P x1 y1) (P x2 y2) inv
              | x1 /= x2 = let m = (y1-y2) * inv -- slightly faster not to `mod` n here
                               x3 = (m*m - x1 - x2) `mod` n
                               y3 = (-y1 + m*(x1 - x3)) `mod` n
                           in P x3 y3
              | x1 == x2 = if (y1 + y2) `elem` [0,n] -- `mod` n == 0  -- includes the case y1 == y2 == 0
                           then Inf
                           else let m = (3*x1*x1 + a) * inv -- slightly faster not to `mod` n here
                                    x3 = (m*m - 2*x1) `mod` n
                                    y3 = (-y1 + m*(x1 - x3)) `mod` n
                                 in P x3 y3

parallelEcSmult _ _ 0 pts = Right $ map (const Inf) pts
parallelEcSmult n ecs k pts | k > 0 = ecSmult' k pts (map (const Inf) pts)
    where -- ecSmult' k qs ps = k * qs + ps
          ecSmult' 0 _ ps = Right ps
          ecSmult' k qs ps = let ps' = if odd k then parallelEcAdd n ecs ps qs else Right ps
                                 qs' = parallelEcAdd n ecs qs qs
                             in case (ps',qs') of
                                (Right ps'', Right qs'') -> ecSmult' (k `div` 2) qs'' ps''
                                (Left _, _) -> ps'
                                (_, Left _) -> qs'

parallelEcTrial n ecs ms pts
    | all (==1) ds = ecTrial' ms pts
    | otherwise = Left $ head $ filter (/=1) ds
    where ds = [gcd n (discriminantEC a b) | EC n a b <- ecs]
          ecTrial' [] pts = Right pts
          ecTrial' (m:ms) pts = case parallelEcSmult n ecs m pts of
                                Right pts' -> ecTrial' ms pts'
                                Left d -> Left d

findFactorParallelECM n | gcd n 6 == 1 =
    let ms = multipliers (sqrt $ fromInteger n)
    in head $ filter ( (/= 0) . (`mod` n) ) $
       lefts [parallelEcTrial n [EC n (a+i) 1 | i <- [1..100]] ms (replicate 100 (P 0 1)) | a <- [0,100..] ]
-- 100 at a time is chosen heuristically.

