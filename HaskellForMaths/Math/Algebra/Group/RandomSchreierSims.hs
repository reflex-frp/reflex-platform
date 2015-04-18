-- Copyright (c) David Amos, 2009. All rights reserved.

module Math.Algebra.Group.RandomSchreierSims where

import System.Random
import Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Control.Monad
import Data.Array.MArray
import Data.Array.IO
import System.IO.Unsafe

import Math.Common.ListSet (toListSet)
import Math.Core.Utils hiding (elts)
import Math.Algebra.Group.PermutationGroup
import Math.Algebra.Group.SchreierSims (sift, cosetRepsGx, ss')


testProdRepl = do (r,xs) <- initProdRepl $ _D 10
                  hs <- replicateM 20 $ nextProdRepl (r,xs)
                  mapM_ print hs

-- Holt p69-71
-- Product replacement algorithm for generating uniformly distributed random elts of a black box group

initProdRepl :: (Ord a, Show a) => [Permutation a] -> IO (Int, IOArray Int (Permutation a))
initProdRepl gs =
    let n = length gs
        r = max 10 n
        xs = (1:) $ take r $ concat $ repeat gs 
    in do xs' <- newListArray (0,r) xs
          replicateM_ 60 $ nextProdRepl (r,xs') -- perform initial mixing
          return (r,xs')

nextProdRepl :: (Ord a, Show a) => (Int, IOArray Int (Permutation a)) -> IO (Maybe (Permutation a))
nextProdRepl (r,xs) =
    do s <- randomRIO (1,r)
       t <- randomRIO (1,r)
       u <- randomRIO (0,3 :: Int)
       out <- updateArray xs s t u
       return out

updateArray xs s t u =
    let (swap,invert) = quotRem u 2 in
    if s == t
    then return Nothing
    else do
        x_0 <- readArray xs 0
        x_s <- readArray xs s
        x_t <- readArray xs t
        let x_s' = mult (swap,invert) x_s x_t
            x_0' = mult (swap,0) x_0 x_s'
        writeArray xs 0 x_0'
        writeArray xs s x_s'
        return (Just x_0')
    where mult (swap,invert) a b = case (swap,invert) of
                                   (0,0) -> a * b
                                   (0,1) -> a * b^-1
                                   (1,0) -> b * a
                                   (1,1) -> b^-1 * a


-- Holt p97-8
-- Random Schreier-Sims algorithm, for finding strong generating set of permutation group

-- It's possible that the following code can be improved by introducing levels only as we need them?

-- |Given generators for a permutation group, return a strong generating set.
-- The result is calculated using random Schreier-Sims algorithm, so has a small (\<10^-6) chance of being incomplete.
-- The sgs is relative to the base implied by the Ord instance.
sgs :: (Ord a, Show a) => [Permutation a] -> [Permutation a]
sgs gs = toListSet $ concatMap snd $ rss gs

rss gs = unsafePerformIO $
    do (r,xs) <- initProdRepl gs
       rss' (r,xs) (initLevels gs) 0

rss' (r,xs) levels i
    | i == 25 = return levels -- stop if we've had 25 successful sifts in a row
    | otherwise = do g <- nextProdRepl (r,xs)
                     let (changed,levels') = updateLevels levels g
                     rss' (r,xs) levels' (if changed then 0 else i+1)
-- if we currently have an sgs for a subgroup of the group, then it must have index >= 2
-- so the chance of a random elt sifting to identity is <= 1/2

initLevels gs = [((b,M.singleton b 1),[]) | b <- bs]
    where bs = toListSet $ concatMap supp gs

updateLevels levels Nothing = (False,levels) -- not strictly correct to increment count on a Nothing
updateLevels levels (Just g) =
    case sift (map fst levels) g of
    Nothing -> (False, levels)
    -- Just 1 -> error "Just 1"
    Just g' -> (True, updateLevels' [] levels g' (minsupp g'))

updateLevels' ls (r@((b,t),s):rs) h b' =
    if b == b'
    then reverse ls ++ ((b, cosetRepsGx (h:s) b), h:s) : rs
    else updateLevels' (r:ls) rs h b'
-- updateLevels' ls [] h b' = error $ "updateLevels: " ++ show (ls,[],h,b')

-- used the following in debugging
-- orderLevels levels = product $ [if M.null t then 1 else toInteger (M.size t) | ((b,t),s) <- levels]


-- recover the base tranversals from the sgs. gs must be an sgs
-- baseTransversalsSGS gs = [let hs = [h | h <- gs, b <= minsupp h] in (b, cosetRepsGx hs b) | b <- bs]
baseTransversalsSGS gs = [let hs = filter ( (b <=) . minsupp ) gs in (b, cosetRepsGx hs b) | b <- bs]
    where bs = toListSet $ map minsupp gs
    -- where bs = toListSet $ concatMap supp gs

-- |Given a strong generating set gs, isMemberSGS gs is a membership test for the group
isMemberSGS :: (Ord a, Show a) => [Permutation a] -> Permutation a -> Bool
isMemberSGS gs h = let bts = baseTransversalsSGS gs in isNothing $ sift bts h


{-
-- Alternative where we carry on with Schreier-Sims when we finish Random Schreier-Sims, just to make sure
-- !! Unfortunately, doesn't appear to work - perhaps ss' doesn't like finding empty levels
sgs2 gs = toListSet $ concatMap snd $ rss2 gs

rss2 gs = unsafePerformIO $
    do (r,xs) <- initProdRepl gs
       levels <- rss' (r,xs) (initLevels gs) 0
       return $ ss' bs (reverse levels) []
    where bs = toListSet $ concatMap supp gs
-}
