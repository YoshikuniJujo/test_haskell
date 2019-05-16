{-# LANGUAGE ScopedTypeVariables #-}

module SieveSt (sieve) where

import Control.Monad.ST
import Data.Proxy
import Data.Array.ST

import SieveGen

sieve :: Int -> [Int]
-- sieve = runST . sieveSt
sieve n = runST $ sieveSt n

sieveSt :: forall s . Int -> ST s [Int]
sieveSt = sieveM (Proxy :: Proxy (STArray s))
