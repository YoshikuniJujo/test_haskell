{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Bits ((.|.), shiftL, shiftR, testBit)
import Data.Bool
import Data.Word

import Circuit

run :: Int -> Circuit -> Circuit
run n = (!! n) . iterate step

type Wires31 = (IWire, IWire, IWire, OWire)

setBits31 :: Wires31 -> Bit -> Bit -> Bit -> Circuit -> Circuit
setBits31 (i1, i2, i3, _) b1 b2 b3 =
	foldr (.) id $ zipWith setBit [i1, i2, i3] [b1, b2, b3]

peekBits31 :: Wires31 -> Circuit -> Bit
peekBits31 (_, _, _, o) = peekOWire o

type Wires22 = (IWire, IWire, OWire, OWire)

setBits22 :: Wires22 -> Bit -> Bit -> Circuit -> Circuit
setBits22 (i1, i2, _, _) b1 b2 = setBit i1 b1 . setBit i2 b2

peekBits22 :: Wires22 -> Circuit -> (Bit, Bit)
peekBits22 (_, _, o1, o2) = (,) <$> peekOWire o1 <*> peekOWire o2

type Wires51 = (IWire, IWire, IWire, IWire, IWire, OWire)

setBits51 :: Wires51 -> Bit -> Bit -> Bit -> Bit -> Bit -> Circuit -> Circuit
setBits51 (i1, i2, i3, i4, i5, _) b1 b2 b3 b4 b5 =
	foldr (.) id $ zipWith setBit [i1, i2, i3, i4, i5] [b1, b2, b3, b4, b5]

peekBits51 :: Wires51 -> Circuit -> Bit
peekBits51 (_, _, _, _, _, o) = peekOWire o

type Wires61 = (IWire, IWire, IWire, IWire, IWire, IWire, OWire)

wordToBits :: Int -> Word64 -> [Bit]
wordToBits n _ | n < 1 = []
wordToBits n w = bool O I (w `testBit` 0) : wordToBits (n - 1) (w `shiftR` 1)

bitsToWord :: [Bit] -> Word64
bitsToWord [] = 0
bitsToWord (b : bs) = (case b of O -> 0; I -> 1) .|. bitsToWord bs `shiftL` 1

listToTuple2 :: [a] -> (a, a)
listToTuple2 [a, b] = (a, b)
listToTuple2 _ = error "Oops!"

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "Oops!"

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [x, y, z, w] = (x, y, z, w)
listToTuple4 _ = error "Oops!"

listToTuple5 :: [a] -> (a, a, a, a, a)
listToTuple5 [x, y, z, w, v] = (x, y, z, w, v)
listToTuple5 _ = error "Oops!"
