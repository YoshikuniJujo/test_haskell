{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module APClosure (printAPClosureGHCi) where

import GHC.Exts.Heap (getClosureData)

function :: Int -> Int -> Int
function x y = x + y

printAPClosureGHCi :: IO ()
printAPClosureGHCi = print =<< getClosureData (function 123)
