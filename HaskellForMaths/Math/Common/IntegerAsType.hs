-- Copyright (c) David Amos, 2009. All rights reserved.

{-# LANGUAGE EmptyDataDecls, ScopedTypeVariables #-}

module Math.Common.IntegerAsType where

class IntegerAsType a where
    value :: a -> Integer

-- multiplication of IntegerAsType
data M a b = M a b
instance (IntegerAsType a, IntegerAsType b) => IntegerAsType (M a b) where
    value _ = value (undefined :: a) * value (undefined :: b)

data TMinus1
instance IntegerAsType TMinus1 where value _ = -1

data TZero
instance IntegerAsType TZero where value _ = 0

data TOne
instance IntegerAsType TOne where value _ = 1

data T2
instance IntegerAsType T2 where value _ = 2

data T3
instance IntegerAsType T3 where value _ = 3

data T5
instance IntegerAsType T5 where value _ = 5

data T7
instance IntegerAsType T7 where value _ = 7

data T11
instance IntegerAsType T11 where value _ = 11

data T13
instance IntegerAsType T13 where value _ = 13

data T17
instance IntegerAsType T17 where value _ = 17

data T19
instance IntegerAsType T19 where value _ = 19

data T23
instance IntegerAsType T23 where value _ = 23

data T29
instance IntegerAsType T29 where value _ = 29

data T31
instance IntegerAsType T31 where value _ = 31

data T37
instance IntegerAsType T37 where value _ = 37

data T41
instance IntegerAsType T41 where value _ = 41

data T43
instance IntegerAsType T43 where value _ = 43

data T47
instance IntegerAsType T47 where value _ = 47

data T53
instance IntegerAsType T53 where value _ = 53

data T59
instance IntegerAsType T59 where value _ = 59

data T61
instance IntegerAsType T61 where value _ = 61

data T67
instance IntegerAsType T67 where value _ = 67

data T71
instance IntegerAsType T71 where value _ = 71

data T73
instance IntegerAsType T73 where value _ = 73

data T79
instance IntegerAsType T79 where value _ = 79

data T83
instance IntegerAsType T83 where value _ = 83

data T89
instance IntegerAsType T89 where value _ = 89

data T97
instance IntegerAsType T97 where value _ = 97

