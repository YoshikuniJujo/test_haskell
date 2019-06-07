module Main where

import TryCircuit

main :: IO ()
main = print . (!! 150) $ mkSampleAluAos64 alu_aos64
