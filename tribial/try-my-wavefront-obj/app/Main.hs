module Main where

import ReadWavefront

main :: IO ()
main = print . readVertices =<< readSample
