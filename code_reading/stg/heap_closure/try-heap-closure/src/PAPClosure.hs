{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PAPClosure (printPAPClosureGHCi) where

import GHC.Exts.Heap (getClosureData)

printPAPClosureGHCi :: IO ()
printPAPClosureGHCi = print =<< getClosureData (+ (3 :: Int))
