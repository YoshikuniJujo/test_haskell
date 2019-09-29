{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PerformanceProblems where

(.++) :: [a] -> [a] -> [a]
[] .++ r = r
(h : t) .++ r = h : t .++ r
