module Main where

import SafeWord

main :: IO ()
main = print $ step3 125

step3 :: SafeWord -> [SafeWord]
step3 Err = []
step3 0 = []
step3 n = n : step3 (n - 3)
