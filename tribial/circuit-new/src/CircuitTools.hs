{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTools where

import Circuit

type Wires31 = (IWire, IWire, IWire, OWire)

setBits31 :: Wires31 -> Bit -> Bit -> Bit -> Circuit -> Circuit
setBits31 (i1, i2, i3, _) b1 b2 b3 cct =
	foldr (uncurry setBit) cct $ zip [i1, i2, i3] [b1, b2, b3]

getBits31 :: Wires31 -> Circuit -> Bit
getBits31 (_, _, _, o) = peekOWire o

run :: Int -> Circuit -> Circuit
run i = (!! i) . iterate step
