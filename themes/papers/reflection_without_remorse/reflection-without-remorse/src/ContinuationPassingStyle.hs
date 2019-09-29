{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ContinuationPassingStyle where

import PerformanceProblems

type DiffList a = [a] -> [a]

abs :: DiffList a -> [a]
abs = ($ [])

rep :: [a] -> DiffList a
rep = (.++)

(.++.) :: DiffList a -> DiffList a -> DiffList a
(.++.) = (.)
