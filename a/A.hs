{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module A where

import GHC.Generics
import qualified Rank2.TH

data A f = A { _a :: f () } deriving Generic
$(Rank2.TH.deriveAll ''A)

