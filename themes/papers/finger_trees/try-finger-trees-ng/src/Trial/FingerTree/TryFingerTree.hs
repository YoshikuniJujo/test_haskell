{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.FingerTree.TryFingerTree where

import Data.FingerTree

sampleFingerTree :: FingerTree Int
sampleFingerTree = mkFingerTree [100 .. 150]
