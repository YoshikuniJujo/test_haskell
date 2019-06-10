{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBits, peekOWire, bitsToWord, wordToBits,
	CircuitBuilder, connectWire, delay,
		andGate, orGate, idGate, notGate, constGate, triStateSelect,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut,
	andGate0, orGate0, notGate0, idGate0, constGate0, connectWire0,
	andGate64, orGate64, notGate64, idGate64, constGate64, connectWire64,
	connectWire0_64,
	setMultBits, peekMultOWires, peekOWire2
	) where

import Data.Word

import CircuitCore

andGate0, orGate0 :: CircuitBuilder (IWire, IWire, OWire)
andGate0 = andGate 1 0 0 0
orGate0 = orGate 1 0 0 0

notGate0, idGate0 :: CircuitBuilder (IWire, OWire)
notGate0 = notGate 1 0 0
idGate0 = idGate 1 0 0

constGate0 :: Bits -> CircuitBuilder OWire
constGate0 bs = constGate bs 1 0 0

connectWire0 :: OWire -> IWire -> CircuitBuilder ()
connectWire0 o i = connectWire (o, 1, 0) (i, 1, 0)

andGate64, orGate64 :: CircuitBuilder (IWire, IWire, OWire)
andGate64 = andGate 64 0 0 0
orGate64 = orGate 64 0 0 0

notGate64, idGate64 :: CircuitBuilder (IWire, OWire)
notGate64 = notGate 64 0 0
idGate64 = idGate 64 0 0

constGate64 :: Bits -> CircuitBuilder OWire
constGate64 bs = constGate bs 64 0 0

connectWire64 :: OWire -> IWire -> CircuitBuilder ()
connectWire64 o i = connectWire (o, 64, 0) (i, 64, 0)

connectWire0_64 :: OWire -> IWire -> CircuitBuilder ()
connectWire0_64 o i = connectWire (o, 1, 0) (i, 64, 0)

setMultBits :: [IWire] -> [Word64] -> Circuit -> Circuit
setMultBits is vs = foldr (.) id $ zipWith setBits is (wordToBits <$> vs)

peekMultOWires :: [OWire] -> Circuit -> [Word64]
peekMultOWires os cct = bitsToWord . (`peekOWire` cct) <$> os

peekOWire2 :: OWire -> OWire -> Circuit -> (Word64, Word64)
peekOWire2 o1 o2 = (,) <$> bitsToWord . peekOWire o1 <*> bitsToWord . peekOWire o2
