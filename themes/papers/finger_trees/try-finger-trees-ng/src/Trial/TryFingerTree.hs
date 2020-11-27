{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryFingerTree where

import Data.List.Range.FingerTree

sampleFingerTree :: FingerTree Int
sampleFingerTree = toTree [100 .. 150]
