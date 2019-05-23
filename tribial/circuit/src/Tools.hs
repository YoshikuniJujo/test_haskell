module Tools where

import Data.Bits
import Data.Bool
import Data.Word

import Circuit

run :: Int -> Circuit -> Circuit
run n = (!! n) . iterate step

listToTuple2 :: [a] -> (a, a)
listToTuple2 [a, b] = (a, b)

wordToBits :: Int -> Word64 -> [Bit]
wordToBits n _ | n < 1 = []
wordToBits n w = bool O I (w `testBit` 0) : wordToBits (n - 1) (w `shiftR` 1)

bitsToWord :: [Bit] -> Word64
bitsToWord [] = 0
bitsToWord (b : bs) = (case b of O -> 0; I -> 1) .|. bitsToWord bs `shiftL` 1
