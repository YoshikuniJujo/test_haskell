module Main where

import Control.Moffy.View.GtkField
import Data.OneOfThem

import Trial.Draw

main :: IO ()
main = runDraw (\wdt cr x -> (SingletonFun (drawLine wdt cr) `apply`) `mapM_` x) sampleLine
