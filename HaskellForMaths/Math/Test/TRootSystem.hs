

module Math.Test.TRootSystem where

import Math.Projects.RootSystem
import qualified Math.Algebra.Group.StringRewriting as SG
import qualified Math.Algebra.Group.SchreierSims as SS

import qualified Data.List as L

test = testStringRewriting && testPermutations

-- tests orders of the groups via presentations
testStringRewriting = all (\(t,n) -> orderWeyl t n == L.genericLength (eltsCoxeter t n))
    [(A,3),(A,4),(A,5),(B,3),(B,4),(B,5),(C,3),(C,4),(C,5),(D,4),(D,5),(F,4),(G,2)]

-- tests orders of the groups via permutations
testPermutations = all (\(t,n) -> orderWeyl t n == SS.order (weylPerms t n))
    [(A,3),(A,4),(A,5),(B,3),(B,4),(B,5),(C,3),(C,4),(C,5),(D,4),(D,5),(E,6),(F,4),(G,2)]