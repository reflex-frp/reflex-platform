-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TNumberTheory.TPrimeFactor where

import Data.List ( (\\) )

import Math.NumberTheory.Prime
import Math.NumberTheory.Factor

import Test.HUnit


testlistPrimeFactor = TestList [
    testlistSmallPrimes,
    testlistMillerRabin,
    testlistMersennePrimes,
    testlistMersenneNonPrimes,
    testlistFermatPrimes,
    testlistFermatNonPrimes,
    testlistCullenPrimes,
    testlistCullenNonPrimes,
    testlistWoodallPrimes,
    testlistWoodallNonPrimes,
    testlistWagstaffPrimes,
    testlistWagstaffNonPrimes,
    testlistFermatFactors,
    testlistNextPrime,
    testlistPrevPrime,
    testlistConsistentFactors,
    testlistFactorOrder
    ]


testlistSmallPrimes = TestList [
    TestCase $ assertEqual "small primes"
               [False,True,True,False,True,False,True,False,False,False]
               (map isPrime [1..10]),
    TestCase $ assertBool "negative primes" (all notPrime [-10..0])
    ]

testcaseMillerRabin n = TestCase $ assertEqual ("MillerRabin " ++ show n)
                                   (isTrialDivisionPrime n) (isMillerRabinPrime n)

testlistMillerRabin = TestList $ map testcaseMillerRabin $ [1 :: Integer ..1000] ++ [10^6..10^6+10^3]

-- Source: http://en.wikipedia.org/wiki/Mersenne_prime
testlistMersennePrimes = TestList
    [TestCase $ assertBool ("Mersenne " ++ show p) (isPrime (2^p-1)) |
     p <- [2,3,5,7,13,17,19,31,61,89,107,127,521] ]

testlistMersenneNonPrimes = TestList
    [TestCase $ assertBool ("Mersenne " ++ show p) (notPrime (2^p-1)) |
     p <- [11,23,29,37,41,43,47,53,59,67,71,73,79,83,97,101,103,109,113] ]

-- http://en.wikipedia.org/wiki/Fermat_prime
testlistFermatPrimes = TestList
    [TestCase $ assertBool ("Fermat " ++ show n) (isPrime (2^2^n + 1)) | n <- [0..4] ]

testlistFermatNonPrimes = TestList
    [TestCase $ assertBool ("Fermat " ++ show n) (notPrime (2^2^n + 1)) | n <- [5..10] ]

-- http://en.wikipedia.org/wiki/Cullen_number
testlistCullenPrimes = TestList
    [TestCase $ assertBool ("Cullen " ++ show n) (isPrime (n * 2^n + 1)) | n <- [141] ]

testlistCullenNonPrimes = TestList
    [TestCase $ assertBool ("Cullen " ++ show n) (notPrime (n * 2^n + 1)) | n <- [2..100] ]

-- http://en.wikipedia.org/wiki/Woodall_number
testlistWoodallPrimes = TestList
    [TestCase $ assertBool ("Woodall " ++ show n) (isPrime (n * 2^n - 1)) |
     n <- [2,3,6,30,75,81,115,123,249,362,384] ]

testlistWoodallNonPrimes = TestList
    [TestCase $ assertBool ("Woodall " ++ show n) (notPrime (n * 2^n - 1)) |
     n <- [2..100] \\ [2,3,6,30,75,81,115,123,249,362,384] ]

-- http://en.wikipedia.org/wiki/Wagstaff_prime
testlistWagstaffPrimes = TestList
    [TestCase $ assertBool ("Wagstaff " ++ show n) (isPrime ((2^n + 1) `div` 3)) |
     n <- [3,5,7,11,13,17,19,23,31,43,61,79,101,127,167,191,199] ]

testlistWagstaffNonPrimes = TestList
    [TestCase $ assertBool ("Wagstaff " ++ show n) (notPrime ((2^n + 1) `div` 3)) |
     n <- takeWhile (<200) primes \\ [3,5,7,11,13,17,19,23,31,43,61,79,101,127,167,191,199] ]


testcaseKnownFactors n ps = TestCase $ assertEqual (show n) ps (pfactors n)

testlistFermatFactors = TestList [
    testcaseKnownFactors (2^2^5+1) [641, 6700417],
    testcaseKnownFactors (2^2^6+1) [274177, 67280421310721]
    ]


testlistNextPrime = TestList
    [TestCase $ assertEqual (show n) p (nextPrime n) |
     (n,p) <- [(0,2),(1,2),(2,3),(3,5),(4,5),(5,7),(6,7),(7,11),(8,11),(9,11),
               (10,11),(11,13),(12,13),(13,17),(14,17),(15,17),(16,17),(17,19),(18,19),(19,23),
               (20,23),(21,23),(22,23),(23,29),(24,29),(25,29),(26,29),(27,29),(28,29),(29,31),(30,31)] ]

testlistPrevPrime = TestList
    [TestCase $ assertEqual (show n) p (prevPrime n) |
     (n,p) <- [(3,2),(4,3),(5,3),(6,5),(7,5),(8,7),(9,7),
               (10,7),(11,7),(12,11),(13,11),(14,13),(15,13),(16,13),(17,13),(18,17),(19,17),
               (20,19),(21,19),(22,19),(23,19),(24,23),(25,23),(26,23),(27,23),(28,23),(29,23),(30,29)] ]


testcaseConsistentFactors n = TestCase $ assertBool (show n) (product (pfactors n) == n)

testlistConsistentFactors = TestList $ map testcaseConsistentFactors $ [10^6..10^6+10^2] ++ [10^16..10^16+10^2]


testlistFactorOrder =
    let f1 = nextPrime 50000; f2 = nextPrime 70000 in TestList [
    TestCase (assertEqual "" [2,2,2,3,3,5] (pfactors (2^3*3^2*5))),
    TestCase (assertEqual "" [f1,f1,f1,f2,f2] (pfactors (f1^3*f2^2))),
    TestCase (assertEqual "" [2,2,2,3,3,5,f1,f1,f1,f2,f2] (pfactors (2^3*3^2*5*f1^3*f2^2)))
    ]
