{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTools where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Bool
import Data.Word

import Circuit

run :: Int -> Circuit -> Circuit
run i = (!! i) . iterate step

peekOWires :: [OWire] -> Circuit -> [Bit]
peekOWires os cct = (`peekOWire` cct) <$> os

type Wires31 = (IWire, IWire, IWire, OWire)

setBits31 :: Wires31 -> Bit -> Bit -> Bit -> Circuit -> Circuit
setBits31 (i1, i2, i3, _) b1 b2 b3 cct =
	foldr (uncurry setBit) cct $ zip [i1, i2, i3] [b1, b2, b3]

getBits31 :: Wires31 -> Circuit -> Bit
getBits31 (_, _, _, o) = peekOWire o

setAndRun31 :: Wires31 -> Bit -> Bit -> Bit -> Int -> Circuit -> Circuit
setAndRun31 ws b1 b2 b3 n = run n . setBits31 ws b1 b2 b3

type Wires22 = (IWire, IWire, OWire, OWire)

setBits22 :: Wires22 -> Bit -> Bit -> Circuit -> Circuit
setBits22 (i1, i2, _, _) b1 b2 cct =
	foldr (uncurry setBit) cct $ zip [i1, i2] [b1, b2]

getBits22 :: Wires22 -> Circuit -> (Bit, Bit)
getBits22 (_, _, o1, o2) = (,) <$> peekOWire o1 <*> peekOWire o2

setAndRun22 :: Wires22 -> Bit -> Bit -> Int -> Circuit -> Circuit
setAndRun22 ws b1 b2 n = run n . setBits22 ws b1 b2

type Wires41 = (IWire, IWire, IWire, IWire, OWire)

setBits41 :: Wires41 -> Bit -> Bit -> Bit -> Bit -> Circuit -> Circuit
setBits41 (i1, i2, i3, i4, _) b1 b2 b3 b4 cct =
	foldr (uncurry setBit) cct $ zip [i1, i2, i3, i4] [b1, b2, b3, b4]

getBits41 :: Wires41 -> Circuit -> Bit
getBits41 (_, _, _, _, o) = peekOWire o

type Wires32 = (IWire, IWire, IWire, OWire, OWire)

setBits32 :: Wires32 -> Bit -> Bit -> Bit -> DoCircuit
setBits32 (i1, i2, i3, _, _) b1 b2 b3 cct =
	foldr (uncurry setBit) cct $ zip [i1, i2, i3] [b1, b2, b3]

getBits32 :: Wires32 -> Circuit -> (Bit, Bit)
getBits32 (_, _, _, o1, o2) = (,) <$> peekOWire o1 <*> peekOWire o2

setAndRun32 :: Wires32 -> Bit -> Bit -> Bit -> Int -> DoCircuit
setAndRun32 ws b1 b2 b3 n = run n . setBits32 ws b1 b2 b3

type WireList21 = ([IWire], [IWire], [OWire])

setBitsL21 :: WireList21 -> Word64 -> Word64 -> DoCircuit
setBitsL21 (i1s, i2s, _) w1 w2 =
	setBits i1s (wordToBits 64 w1) . setBits i2s (wordToBits 64 w2)

getBitsL21 :: WireList21 -> Circuit -> [Bit]
getBitsL21 (_, _, os) cct = (`peekOWire` cct) <$> os

getWordL21 :: WireList21 -> Circuit -> Word64
getWordL21 ws = bitsToWord . getBitsL21 ws

setAndRunL21 :: WireList21 -> Word64 -> Word64 -> Int -> DoCircuit
setAndRunL21 ws w1 w2 n = run n . setBitsL21 ws w1 w2

setAndBitsL21 :: WireList21 -> Word64 -> Word64 -> Int -> Circuit -> [Bit]
setAndBitsL21 ws w1 w2 n = getBitsL21 ws . setAndRunL21 ws w1 w2 n

setAndWordL21 :: WireList21 -> Word64 -> Word64 -> Int -> Circuit -> Word64
setAndWordL21 ws w1 w2 n = getWordL21 ws . setAndRunL21 ws w1 w2 n

wordToBits :: Word8 -> Word64 -> [Bit]
wordToBits 0 _ = []
wordToBits n w = bool O I (w .&. 1 /= 0) : wordToBits (n - 1) (w `shiftR` 1)

bitsToWord :: [Bit] -> Word64
bitsToWord [] = 0
bitsToWord (O : bs) = bitsToWord bs `shiftL` 1
bitsToWord (I : bs) = bitsToWord bs `shiftL` 1 .|. 1
bitsToWord _ = error "bitsToWord: not number"

bitsToWordMaybe :: [Bit] -> Maybe Word64
bitsToWordMaybe [] = Just 0
bitsToWordMaybe (O : bs) = (`shiftL` 1) <$> bitsToWordMaybe bs
bitsToWordMaybe (I : bs) = (.|. 1) . (`shiftL` 1) <$> bitsToWordMaybe bs
bitsToWordMaybe _ = Nothing
