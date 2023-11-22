module Main where

import Control.Moffy
import Trial.TryMultiWindow

main :: IO ()
main = print =<< runTryMultiWindow threeWindows
