{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module BucketSortSt where

import Control.Monad.ST
import Data.Array.ST

import BucketSort

bucketSort :: (Ix a, Enum a) => (a, a) -> [a] -> [a]
bucketSort bs xs = runST $ bucketSt bs xs

bucketSt :: forall s a . (Ix a, Enum a) => (a, a) -> [a] -> ST s [a]
bucketSt = bucket @(STArray s)
