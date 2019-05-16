{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module SieveSt (sieve) where

import Control.Monad.ST
import Data.Array.ST

import SieveGen

sieve :: Int -> [Int]
-- sieve = runST . sieveSt
sieve n = runST $ sieveSt n

sieveSt :: forall s . Int -> ST s [Int]
sieveSt = sieveM @(STArray s)
