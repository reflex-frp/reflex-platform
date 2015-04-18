-- Copyright (c) 2006-2011, David Amos. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}

-- |A module providing functions to test for primality, and find next and previous primes.
module Math.NumberTheory.Prime (primes, isTrialDivisionPrime, isMillerRabinPrime,
                                isPrime, notPrime, prevPrime, nextPrime) where

import System.Random
import System.IO.Unsafe


isTrialDivisionPrime n
    | n > 1 = isNotDivisibleBy primes
    | otherwise = False
    where isNotDivisibleBy (d:ds) | d*d > n         = True
                                  | n `rem` d == 0  = False
                                  | otherwise       = isNotDivisibleBy ds

-- |A (lazy) list of the primes
primes :: [Integer]
primes = 2 : 3 : filter isTrialDivisionPrime (concat [ [m6-1,m6+1] | m6 <- [6,12..] ])

-- initial version. This isn't going to be very good if n has any "large" prime factors (eg > 10000)
pfactors1 n | n > 0 = pfactors' n primes
            | n < 0 = -1 : pfactors' (-n) primes
    where pfactors' n (d:ds) | n == 1 = []
                             | n < d*d = [n]
                             | r == 0 = d : pfactors' q (d:ds)
                             | otherwise = pfactors' n ds
                             where (q,r) = quotRem n d


-- MILLER-RABIN TEST
-- Cohen, A Course in Computational Algebraic Number Theory, p422
-- Koblitz, A Course in Number Theory and Cryptography


-- Let n-1 = 2^s * q, q odd
-- Then n is a strong pseudoprime to base b if
-- either b^q == 1 (mod n)
-- or b^(2^r * q) == -1 (mod n) for some 0 <= r < s
-- (For we know that if n is prime, then b^(n-1) == 1 (mod n)

isStrongPseudoPrime n b =
    let (s,q) = split2s 0 (n-1)  -- n-1 == 2^s * q, with q odd
    in isStrongPseudoPrime' n (s,q) b

isStrongPseudoPrime' n (s,q) b
    | b' == 1 = True
    | otherwise = n-1 `elem` squarings
    where b' = power_mod b q n     -- b' = b^q `mod` n
          squarings = take s $ iterate (\x -> x*x `mod` n) b' -- b^(2^r *q) for 0 <= r < s

-- split2s 0 m returns (s,t) such that 2^s * t == m, t odd
split2s s t = let (q,r) = t `quotRem` 2
              in if r == 0 then split2s (s+1) q else (s,t)

-- power_mod b t n == b^t mod n
power_mod b t n = powerMod' b 1 t
    where powerMod' x y 0 = y
          powerMod' x y t = powerMod' (x*x `rem` n) (if even t then y else x*y `rem` n) (t `div` 2)

isMillerRabinPrime' n
    | n >= 4 =
        let (s,q) = split2s 0 (n-1) -- n-1 == 2^s * q, with q odd
        in do g <- getStdGen
              let rs = randomRs (2,n-1) g
              return $ all (isStrongPseudoPrime' n (s,q)) (take 25 rs)
    | n >= 2 = return True
    | otherwise = return False
-- Cohen states that if we restrict our rs to single word numbers, we can use a more efficient powering algorithm

-- isMillerRabinPrime :: Integer -> Bool
isMillerRabinPrime n = unsafePerformIO (isMillerRabinPrime' n)


-- |Is this number prime? The algorithm consists of using trial division to test for very small factors,
-- followed if necessary by the Miller-Rabin probabilistic test.
isPrime :: Integer -> Bool
isPrime n | n > 1 = isPrime' $ takeWhile (< 100) primes
          | otherwise = False
    where isPrime' (d:ds) | n < d*d = True
                          | otherwise = let (q,r) = quotRem n d
                                        in if r == 0 then False else isPrime' ds
          isPrime' [] = isMillerRabinPrime n
-- the < 100 is found heuristically to be about the point at which trial division stops being worthwhile

notPrime :: Integer -> Bool
notPrime = not . isPrime

-- |Given n, @prevPrime n@ returns the greatest p, p < n, such that p is prime
prevPrime :: Integer -> Integer
prevPrime n | n > 5 = head $ filter isPrime $ candidates
            | n < 3 = error "prevPrime: no previous primes"
            | n == 3 = 2
            | otherwise = 3
            where n6 = (n `div` 6) * 6
                  candidates = dropWhile (>= n) $ concat [ [m6+5,m6+1] | m6 <- [n6, n6-6..] ]

-- |Given n, @nextPrime n@ returns the least p, p > n, such that p is prime
nextPrime :: Integer -> Integer
nextPrime n | n < 2 = 2
            | n < 3 = 3
            | otherwise = head $ filter isPrime $ candidates
            where n6 = (n `div` 6) * 6
                  candidates = dropWhile (<= n) $ concat [ [m6+1,m6+5] | m6 <- [n6, n6+6..] ]

-- slightly better version. This is okay so long as n has at most one "large" prime factor (> 10000)
-- if it has more, it does at least tell you, via an error message, that it has run into difficulties
pfactors2 n | n > 0 = pfactors' n $ takeWhile (< 10000) primes
            | n < 0 = -1 : pfactors' (-n) (takeWhile (< 10000) primes)
    where pfactors' n (d:ds) | n == 1 = []
                             | n < d*d = [n]
                             | r == 0 = d : pfactors' q (d:ds)
                             | otherwise = pfactors' n ds
                             where (q,r) = quotRem n d
          pfactors' n [] = if isMillerRabinPrime n then [n] else error ("pfactors2: can't factor " ++ show n)

