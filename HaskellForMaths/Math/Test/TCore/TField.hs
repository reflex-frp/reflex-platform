-- Copyright (c) 2011, David Amos. All rights reserved.

module Math.Test.TCore.TField where

import Test.QuickCheck
import Math.Core.Field

prop_Field (x,y,z) =
    (x+y)+z == x+(y+z)                          &&  -- associativity of addition
    (x+0) == x && (0+x) == x                    &&  -- identity for addition
    x+(-x) == 0 && (-x)+x == 0                  &&  -- additive inverse
    x+y == y+x                                  &&  -- commutativity of addition
    (x*y)*z == x*(y*z)                          &&  -- associativity of multiplication
    (x*1) == x && (1*x) == x                    &&  -- identity for multiplication
    (x == 0 || (x*(1/x) == 1 && (1/x)*x == 1))  &&  -- multiplicative inverse
    x*y == y*x                                  &&  -- commutativity of multiplication
    x*(y+z) == x*y + x*z && (x+y)*z == x*z + y*z    -- distributivity of multiplication over addition

instance Arbitrary F2 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F3 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F5 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F7 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F11 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F13 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F17 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F19 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F23 where
    arbitrary = do {n <- arbitrary; return (fromInteger n)}

instance Arbitrary F4 where
    arbitrary = do {n <- arbitrary; return (f4 !! mod (fromInteger n) 4)}

instance Arbitrary F8 where
    arbitrary = do {n <- arbitrary; return (f8 !! mod (fromInteger n) 8)}

instance Arbitrary F9 where
    arbitrary = do {n <- arbitrary; return (f9 !! mod (fromInteger n) 9)}

instance Arbitrary F16 where
    arbitrary = do {n <- arbitrary; return (f16 !! mod (fromInteger n) 16)}

instance Arbitrary F25 where
    arbitrary = do {n <- arbitrary; return (f25 !! mod (fromInteger n) 25)}


quickCheckField =
    do putStrLn "Testing finite fields"
       putStrLn "Testing F2..."
       quickCheck (prop_Field :: (F2,F2,F2) -> Bool)
       putStrLn "Testing F3..."
       quickCheck (prop_Field :: (F3,F3,F3) -> Bool)
       putStrLn "Testing F5..."
       quickCheck (prop_Field :: (F5,F5,F5) -> Bool)
       putStrLn "Testing F7..."
       quickCheck (prop_Field :: (F7,F7,F7) -> Bool)
       putStrLn "Testing F11..."
       quickCheck (prop_Field :: (F11,F11,F11) -> Bool)
       putStrLn "Testing F13..."
       quickCheck (prop_Field :: (F13,F13,F13) -> Bool)
       putStrLn "Testing F17..."
       quickCheck (prop_Field :: (F17,F17,F17) -> Bool)
       putStrLn "Testing F19..."
       quickCheck (prop_Field :: (F19,F19,F19) -> Bool)
       putStrLn "Testing F23..."
       quickCheck (prop_Field :: (F23,F23,F23) -> Bool)
       putStrLn "Testing F4..."
       quickCheck (prop_Field :: (F4,F4,F4) -> Bool)
       putStrLn "Testing F8..."
       quickCheck (prop_Field :: (F8,F8,F8) -> Bool)
       putStrLn "Testing F9..."
       quickCheck (prop_Field :: (F9,F9,F9) -> Bool)
       putStrLn "Testing F16..."
       quickCheck (prop_Field :: (F16,F16,F16) -> Bool)
       putStrLn "Testing F25..."
       quickCheck (prop_Field :: (F25,F25,F25) -> Bool)
