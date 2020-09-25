module Main where

import Trial.TryDefaultWindow

main :: IO ()
main = print =<< runTryDefaultWindow tryDefaultWindow
