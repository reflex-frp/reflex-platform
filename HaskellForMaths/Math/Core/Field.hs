-- Copyright (c) 2011, David Amos. All rights reserved.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |A module defining the field Q of rationals and the small finite fields F2, F3, F4, F5, F7, F8, F9, F11, F13, F16, F17, F19, F23, F25.
--
-- Given a prime power q, Fq is the type representing elements of the field (eg @F4@),
-- fq is a list of the elements of the field, beginning 0,1,... (eg @f4@),
-- and for prime power fields, aq is a primitive element, which generates the multiplicative group (eg @a4@).
--
-- The design philosophy is that fq, the list of elements, represents the field.
-- Thus, many functions elsewhere in the library expect to take fq as an argument,
-- telling them which field to work over.
module Math.Core.Field where

import Data.Ratio
import Data.Bits
import Data.List as L

import Math.Core.Utils (FinSet, elts)

-- |Q is just the rationals, but with a better show function than the Prelude version
newtype Q = Q Rational deriving (Eq,Ord,Num,Fractional)

instance Show Q where
    show (Q x) | b == 1    = show a
               | otherwise = show a ++ "/" ++ show b
               where a = numerator x
                     b = denominator x


numeratorQ (Q x) = Data.Ratio.numerator x
denominatorQ (Q x) = Data.Ratio.denominator x


-- The following implementations of the prime fields are only slightly faster than the versions in Math.Algebra.Field.Base

-- |F2 is a type for the finite field with 2 elements
newtype F2 = F2 Int deriving (Eq,Ord)

instance Show F2 where
    show (F2 x) = show x

instance Num F2 where
    F2 x + F2 y = F2 $ (x+y) .&. 1 -- `mod` 2
    negate x = x
    F2 x * F2 y = F2 $ x*y
    fromInteger n = F2 $ fromInteger n `mod` 2
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F2 where
    recip (F2 0) = error "F2.recip 0"
    recip (F2 1) = F2 1
    fromRational _ = error "F2.fromRational: not well defined"

instance FinSet F2 where elts = f2

-- |f2 is a list of the elements of F2
f2 :: [F2]
f2 = map fromInteger [0..1] -- :: [F2]


-- |F3 is a type for the finite field with 3 elements
newtype F3 = F3 Int deriving (Eq,Ord)

instance Show F3 where
    show (F3 x) = show x

instance Num F3 where
    F3 x + F3 y = F3 $ (x+y) `mod` 3
    negate (F3 0) = F3 0
    negate (F3 x) = F3 $ 3 - x
    F3 x * F3 y = F3 $ (x*y) `mod` 3
    fromInteger n = F3 $ fromInteger n `mod` 3
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F3 where
    recip (F3 0) = error "F3.recip 0"
    recip (F3 x) = F3 x
    fromRational _ = error "F3.fromRational: not well defined"

instance FinSet F3 where elts = f3

-- |f3 is a list of the elements of F3
f3 :: [F3]
f3 = map fromInteger [0..2] -- :: [F3]


-- |F5 is a type for the finite field with 5 elements
newtype F5 = F5 Int deriving (Eq,Ord)

instance Show F5 where
    show (F5 x) = show x

instance Num F5 where
    F5 x + F5 y = F5 $ (x+y) `mod` 5
    negate (F5 0) = F5 0
    negate (F5 x) = F5 $ 5 - x
    F5 x * F5 y = F5 $ (x*y) `mod` 5
    fromInteger n = F5 $ fromInteger n `mod` 5
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F5 where
    recip (F5 0) = error "F5.recip 0"
    recip (F5 x) = F5 $ (x^3) `mod` 5
    fromRational _ = error "F5.fromRational: not well defined"

instance FinSet F5 where elts = f5

-- |f5 is a list of the elements of F5
f5 :: [F5]
f5 = map fromInteger [0..4]


-- |F7 is a type for the finite field with 7 elements
newtype F7 = F7 Int deriving (Eq,Ord)

instance Show F7 where
    show (F7 x) = show x

instance Num F7 where
    F7 x + F7 y = F7 $ (x+y) `mod` 7
    negate (F7 0) = F7 0
    negate (F7 x) = F7 $ 7 - x
    F7 x * F7 y = F7 $ (x*y) `mod` 7
    fromInteger n = F7 $ fromInteger n `mod` 7
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F7 where
    recip (F7 0) = error "F7.recip 0"
    recip (F7 x) = F7 $ (x^5) `mod` 7
    fromRational _ = error "F7.fromRational: not well defined"

instance FinSet F7 where elts = f7

-- |f7 is a list of the elements of F7
f7 :: [F7]
f7 = map fromInteger [0..6]


-- |F11 is a type for the finite field with 11 elements
newtype F11 = F11 Int deriving (Eq,Ord)

instance Show F11 where
    show (F11 x) = show x

instance Num F11 where
    F11 x + F11 y = F11 $ (x+y) `mod` 11
    negate (F11 0) = F11 0
    negate (F11 x) = F11 $ 11 - x
    F11 x * F11 y = F11 $ (x*y) `mod` 11
    fromInteger n = F11 $ fromInteger n `mod` 11
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F11 where
    recip (F11 0) = error "F11.recip 0"
    recip (F11 x) = F11 $ (x^9) `mod` 11
    fromRational _ = error "F11.fromRational: not well defined"

instance FinSet F11 where elts = f11

-- |f11 is a list of the elements of F11
f11 :: [F11]
f11 = map fromInteger [0..10]


-- |F13 is a type for the finite field with 13 elements
newtype F13 = F13 Int deriving (Eq,Ord)

instance Show F13 where
    show (F13 x) = show x

instance Num F13 where
    F13 x + F13 y = F13 $ (x+y) `mod` 13
    negate (F13 0) = F13 0
    negate (F13 x) = F13 $ 13 - x
    F13 x * F13 y = F13 $ (x*y) `mod` 13
    fromInteger n = F13 $ fromInteger n `mod` 13
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F13 where
    recip (F13 0) = error "F13.recip 0"
    recip (F13 x) = F13 $ (x5*x5*x) `mod` 13 where x5 = x^5 `mod` 13 -- 12^11 would overflow Int
    fromRational _ = error "F13.fromRational: not well defined"

instance FinSet F13 where elts = f13

-- |f13 is a list of the elements of F13
f13 :: [F13]
f13 = map fromInteger [0..12]


-- |F17 is a type for the finite field with 17 elements
newtype F17 = F17 Int deriving (Eq,Ord)

instance Show F17 where
    show (F17 x) = show x

instance Num F17 where
    F17 x + F17 y = F17 $ (x+y) `mod` 17
    negate (F17 0) = F17 0
    negate (F17 x) = F17 $ 17 - x
    F17 x * F17 y = F17 $ (x*y) `mod` 17
    fromInteger n = F17 $ fromInteger n `mod` 17
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F17 where
    recip (F17 0) = error "F17.recip 0"
    recip (F17 x) = F17 $ (x5^3) `mod` 17 where x5 = x^5 `mod` 17 -- 16^15 would overflow Int
    fromRational _ = error "F17.fromRational: not well defined"

instance FinSet F17 where elts = f17

-- |f17 is a list of the elements of F17
f17 :: [F17]
f17 = map fromInteger [0..16]


-- |F19 is a type for the finite field with 19 elements
newtype F19 = F19 Int deriving (Eq,Ord)

instance Show F19 where
    show (F19 x) = show x

instance Num F19 where
    F19 x + F19 y = F19 $ (x+y) `mod` 19
    negate (F19 0) = F19 0
    negate (F19 x) = F19 $ 19 - x
    F19 x * F19 y = F19 $ (x*y) `mod` 19
    fromInteger n = F19 $ fromInteger n `mod` 19
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F19 where
    recip (F19 0) = error "F17.recip 0"
    recip (F19 x) = F19 $ (x4^4*x) `mod` 19 where x4 = x^4 `mod` 19 -- 18^17 would overflow Int
    fromRational _ = error "F19.fromRational: not well defined"

instance FinSet F19 where elts = f19

-- |f19 is a list of the elements of F19
f19 :: [F19]
f19 = map fromInteger [0..18]


-- |F23 is a type for the finite field with 23 elements
newtype F23 = F23 Int deriving (Eq,Ord)

instance Show F23 where
    show (F23 x) = show x

instance Num F23 where
    F23 x + F23 y = F23 $ (x+y) `mod` 23
    negate (F23 0) = F23 0
    negate (F23 x) = F23 $ 23 - x
    F23 x * F23 y = F23 $ (x*y) `mod` 23
    fromInteger n = F23 $ fromInteger n `mod` 23
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F23 where
    recip (F23 0) = error "F23.recip 0"
    recip (F23 x) = F23 $ (x5^4*x) `mod` 23 where x5 = x^5 `mod` 23 -- 22^21 would overflow Int
    fromRational _ = error "F23.fromRational: not well defined"

instance FinSet F23 where elts = f23

-- |f23 is a list of the elements of F23
f23 :: [F23]
f23 = map fromInteger [0..22]


-- The following implementations of the prime power fields are significantly faster than the versions in Math.Algebra.Field.Extension

-- |F4 is a type for the finite field with 4 elements.
-- F4 is represented as the extension of F2 by an element a4 satisfying x^2+x+1 = 0
newtype F4 = F4 Int deriving (Eq,Ord)

instance Show F4 where
    show (F4 0x00) = "0"
    show (F4 0x01) = "1"
    show (F4 0x10) = "a4"
    show (F4 0x11) = "a4+1" -- == a4^2

-- |a4 is a primitive element for F4 as an extension over F2. a4 satisfies x^2+x+1 = 0.
a4 :: F4
a4 = F4 0x10

instance Num F4 where
    F4 x + F4 y = F4 $ (x+y) .&. 0x11
    negate x = x
    F4 x * F4 y = let z = x*y in
                  if z `testBit` 8
                  then F4 ((z + 0x11) .&. 0x11) -- this is replacing x^2 by x+1
                  else F4 z
    fromInteger n = F4 $ fromInteger n .&. 1
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F4 where
    recip (F4 0) = error "F4.recip 0"
    recip (F4 1) = F4 1
    recip (F4 x) = F4 (x `xor` 1)
    fromRational _ = error "F4.fromRational: not well defined"

instance FinSet F4 where elts = f4

-- |f4 is a list of the elements of F4
f4 :: [F4]
f4 = L.sort $ 0 : powers a4

powers x | x /= 0 = 1 : takeWhile (/=1) (iterate (*x) x)


-- |F8 is a type for the finite field with 8 elements.
-- F8 is represented as the extension of F2 by an element a8 satisfying x^3+x+1 = 0
newtype F8 = F8 Int deriving (Eq,Ord)

instance Show F8 where
    show (F8 0x0) = "0"
    show (F8 0x1) = "1"
    show (F8 0x10) = "a8"
    show (F8 0x11) = "a8+1"
    show (F8 0x100) = "a8^2"
    show (F8 0x101) = "a8^2+1"
    show (F8 0x110) = "a8^2+a8"
    show (F8 0x111) = "a8^2+a8+1"

-- |a8 is a primitive element for F8 as an extension over F2. a8 satisfies x^3+x+1 = 0.
a8 :: F8
a8 = F8 0x10

instance Num F8 where
    F8 x + F8 y = F8 $ (x+y) .&. 0x111
    negate x = x
    F8 x * F8 y = F8 $ ((z43 `shiftR` 8) + (z43 `shiftR` 12) + z) .&. 0x111
        where z = x*y; z43 = z .&. 0xff000; -- z210 = z .&. 0xfff
        -- Explanation: We are making the substitution x^3 = x+1, x^4 = x^2+x
    fromInteger n = F8 $ fromInteger n .&. 0x1
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F8 where
    recip (F8 0) = error "F8.recip 0"
    recip x = x^6
    fromRational _ = error "F8.fromRational: not well defined"

instance FinSet F8 where elts = f8

-- |f8 is a list of the elements of F8
f8 :: [F8]
f8 = L.sort $ 0 : powers a8


-- |F9 is a type for the finite field with 9 elements.
-- F9 is represented as the extension of F3 by an element a9 satisfying x^2+2x+2 = 0
newtype F9 = F9 Int deriving (Eq,Ord)

instance Show F9 where
    show (F9 0x00) = "0"
    show (F9 0x01) = "1"
    show (F9 0x02) = "2"
    show (F9 0x100) = "a9"
    show (F9 0x101) = "a9+1"
    show (F9 0x102) = "a9+2"
    show (F9 0x200) = "2a9"
    show (F9 0x201) = "2a9+1"
    show (F9 0x202) = "2a9+2"

-- |a9 is a primitive element for F9 as an extension over F3. a9 satisfies x^2+2x+2 = 0.
a9 :: F9
a9 = F9 0x100

instance Num F9 where
    F9 x + F9 y = F9 $ z1 + z0
        where z = x+y; z1 = (z .&. 0xff00) `mod` 0x300; z0 = (z .&. 0xff) `mod` 3
    negate (F9 x) = F9 $ z1 + z0
        where z = 0x303 - x; z1 = (z .&. 0xff00) `mod` 0x300; z0 = (z .&. 0xff) `mod` 3
    F9 x * F9 y = F9 $ ((z2 + z1) `mod` 0x300) + ((z2 + z0) `mod` 3) 
        where z = x*y; z2 = z .&. 0xff0000; z1 = z .&. 0xff00; z0 = z .&. 0xff
        -- Explanation: We are substituting x^2 = x+1.
        -- We could do z2 `shiftR` 8 and z2 `shiftR` 16
        -- However, because 0x100 `mod` 3 == 1, we don't need to 
    fromInteger n = F9 $ fromInteger n `mod` 3
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F9 where
    recip (F9 0) = error "F9.recip 0"
    recip x = x^7
    fromRational _ = error "F9.fromRational: not well defined"

instance FinSet F9 where elts = f9

-- |f9 is a list of the elements of F9
f9 :: [F9]
f9 = L.sort $ 0 : powers a9


-- |F16 is a type for the finite field with 16 elements.
-- F16 is represented as the extension of F2 by an element a16 satisfying x^4+x+1 = 0
newtype F16 = F16 Int deriving (Eq,Ord)

instance Show F16 where
    show (F16 0x0) = "0"
    show (F16 0x1) = "1"
    show (F16 0x10) = "a16"
    show (F16 0x11) = "a16+1"
    show (F16 0x100) = "a16^2"
    show (F16 0x101) = "a16^2+1"
    show (F16 0x110) = "a16^2+a16"
    show (F16 0x111) = "a16^2+a16+1"
    show (F16 0x1000) = "a16^3"
    show (F16 0x1001) = "a16^3+1"
    show (F16 0x1010) = "a16^3+a16"
    show (F16 0x1011) = "a16^3+a16+1"
    show (F16 0x1100) = "a16^3+a16^2"
    show (F16 0x1101) = "a16^3+a16^2+1"
    show (F16 0x1110) = "a16^3+a16^2+a16"
    show (F16 0x1111) = "a16^3+a16^2+a16+1"

-- |a16 is a primitive element for F16 as an extension over F2. a16 satisfies x^4+x+1 = 0.
a16 :: F16
a16 = F16 0x10

instance Num F16 where
    F16 x + F16 y = F16 $ (x+y) .&. 0x1111
    negate x = x
    F16 x * F16 y = F16 $ ((z654 `shiftR` 12) + (z654 `shiftR` 16) + z) .&. 0x1111
        where z = x*y; z654 = z .&. 0xfff0000; -- z3210 = z .&. 0xffff
        -- Explanation: We are making the substitution x^4 = x+1 (and also for x^5, x^6)
    fromInteger n = F16 $ fromInteger n .&. 0x1
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F16 where
    recip (F16 0) = error "F16.recip 0"
    recip x = x^14
    fromRational _ = error "F16.fromRational: not well defined"

instance FinSet F16 where elts = f16

-- |f16 is a list of the elements of F16
f16 :: [F16]
f16 = L.sort $ 0 : powers a16


-- |F25 is a type for the finite field with 25 elements.
-- F25 is represented as the extension of F5 by an element a25 satisfying x^2+4x+2 = 0
newtype F25 = F25 Int deriving (Eq,Ord)

instance Show F25 where
    show (F25 x) = case ( (x .&. 0xff00) `shiftR` 8, x .&. 0xff ) of
                   (0,x0) -> show x0
                   (1,0) -> "a25"
                   (1,x0) -> "a25+" ++ show x0
                   (x1,0) -> show x1 ++ "a25"
                   (x1,x0) -> show x1 ++ "a25+" ++ show x0

-- |a25 is a primitive element for F25 as an extension over F5. a25 satisfies x^2+4x+2 = 0.
a25 :: F25
a25 = F25 0x100

instance Num F25 where
    F25 x + F25 y = F25 $ z1 + z0
        where z = x+y; z1 = (z .&. 0xff00) `mod` 0x500; z0 = (z .&. 0xff) `mod` 5
    negate (F25 x) = F25 $ z1 + z0
        where z = 0x505 - x; z1 = (z .&. 0xff00) `mod` 0x500; z0 = (z .&. 0xff) `mod` 5
    F25 x * F25 y = F25 $ ((z2 + z1) `mod` 0x500) + ((3*z2 + z0) `mod` 5) 
        where z = x*y; z2 = z .&. 0xff0000; z1 = z .&. 0xff00; z0 = z .&. 0xff
        -- Explanation: We are substituting x^2 = x+3.
        -- We could do z2 `shiftR` 8 and z2 `shiftR` 16
        -- However, because 0x100 `mod` 5 == 1, we don't need to 
    fromInteger n = F25 $ fromInteger n `mod` 5
    abs _ = error "Prelude.Num.abs: inappropriate abstraction"
    signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance Fractional F25 where
    recip (F25 0) = error "F25.recip 0"
    recip x = x^23
    fromRational _ = error "F25.fromRational: not well defined"

instance FinSet F25 where elts = f25

-- |f25 is a list of the elements of F25
f25 :: [F25]
f25 = L.sort $ 0 : powers a25

