module Main where

import TryCircuit

main :: IO ()
main = print . (!! 45) $ mkSampleAluAos64 alu_aos64'
