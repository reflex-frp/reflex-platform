

module Math.Common.ListSet where

import Data.List (group,sort)
-- versions of Data.List functions which assume that the lists are ascending sets (no repeated elements)

toListSet xs = map head $ group $ sort xs

isListSet (x1:x2:xs) = x1 < x2 && isListSet (x2:xs)
isListSet _ = True


union (x:xs) (y:ys) =
    case compare x y of
    LT -> x : union xs (y:ys)
    EQ -> x : union xs ys
    GT -> y : union (x:xs) ys
union [] ys = ys
union xs [] = xs

intersect (x:xs) (y:ys) =
    case compare x y of
    LT -> intersect xs (y:ys)
    EQ -> x : intersect xs ys
    GT -> intersect (x:xs) ys
intersect _ _ = []

(x:xs) \\ (y:ys) =
    case compare x y of
    LT -> x : (xs \\ (y:ys))
    EQ -> xs \\ ys
    GT -> (x:xs) \\ ys
[] \\ _ = []
xs \\ [] = xs

symDiff (x:xs) (y:ys) =
    case compare x y of
    LT -> x : symDiff xs (y:ys)
    EQ -> symDiff xs ys
    GT -> y : symDiff (x:xs) ys
symDiff [] ys = ys
symDiff xs [] = xs

disjoint xs ys = null (intersect xs ys)

isSubset (x:xs) (y:ys) =
    case compare x y of
    LT -> False
    EQ -> isSubset xs ys
    GT -> isSubset (x:xs) ys
isSubset [] _ = True
isSubset _ [] = False

-- Note that an ListSet.elem turned out to be slower than Data.List.elem
-- (Perhaps because it's slower when x `notElem` xs)
