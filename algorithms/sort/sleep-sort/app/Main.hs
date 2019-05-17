module Main where

import SleepSort

main :: IO ()
main = print =<< sleepSort 100 [99, 75, 3, 4, 15, 21, 22, 22, 23, 50, 51, 42]
