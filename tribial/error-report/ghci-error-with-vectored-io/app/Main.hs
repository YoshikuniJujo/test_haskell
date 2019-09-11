module Main where

import TryVectoredIo

main :: IO ()
main = tryWritev >> tryReadv
