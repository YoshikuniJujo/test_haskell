{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BucketSort where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.IO

import BucketSortM

bucketSort :: Ix i => (i, i) -> [i] -> [i]
bucketSort bs is = runST $ bucketSortST bs is

bucketSortST :: forall s i . Ix i => (i, i) -> [i] -> ST s [i]
bucketSortST = bucketSortM @(STArray s)

bucketSortIO :: Ix i => (i, i) -> [i] -> IO [i]
bucketSortIO = bucketSortM @IOArray

bsort :: Ix i => (x -> i) -> (i, i) -> [x] -> [x]
bsort getIx bs xs = runST $ bsortST getIx bs xs

bsortST :: forall s i x . Ix i => (x -> i) -> (i, i) -> [x] -> ST s [x]
bsortST = bsortM @(STArray s) @(STArray s)

bsortIO :: Ix i => (x -> i) -> (i, i) -> [x] -> IO [x]
bsortIO = bsortM @IOArray @IOArray
