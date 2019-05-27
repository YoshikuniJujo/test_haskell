{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTools where

import Circuit

run :: Int -> Circuit -> Circuit
run i = (!! i) . iterate step

type Wires31 = (IWire, IWire, IWire, OWire)

setBits31 :: Wires31 -> Bit -> Bit -> Bit -> Circuit -> Circuit
setBits31 (i1, i2, i3, _) b1 b2 b3 cct =
	foldr (uncurry setBit) cct $ zip [i1, i2, i3] [b1, b2, b3]

getBits31 :: Wires31 -> Circuit -> Bit
getBits31 (_, _, _, o) = peekOWire o

type Wires41 = (IWire, IWire, IWire, IWire, OWire)

setBits41 :: Wires41 -> Bit -> Bit -> Bit -> Bit -> Circuit -> Circuit
setBits41 (i1, i2, i3, i4, _) b1 b2 b3 b4 cct =
	foldr (uncurry setBit) cct $ zip [i1, i2, i3, i4] [b1, b2, b3, b4]

getBits41 :: Wires41 -> Circuit -> Bit
getBits41 (_, _, _, _, o) = peekOWire o
