{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBits, peekOWire, bitsToWord, wordToBits,
	CircuitBuilder, connectWire,
		andGate, orGate, idGate, notGate, constGate,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut,
	andGate0, orGate0, notGate0, idGate0, connectWire0,
	andGate64, orGate64, notGate64, idGate64, connectWire64,
	connectWire0_64
	) where

import CircuitCore

andGate0, orGate0 :: CircuitBuilder (IWire, IWire, OWire)
andGate0 = andGate 1 0 0 0
orGate0 = orGate 1 0 0 0

notGate0, idGate0 :: CircuitBuilder (IWire, OWire)
notGate0 = notGate 1 0 0
idGate0 = idGate 1 0 0

connectWire0 :: OWire -> IWire -> CircuitBuilder ()
connectWire0 o i = connectWire (o, 1, 0) (i, 1, 0)

andGate64, orGate64 :: CircuitBuilder (IWire, IWire, OWire)
andGate64 = andGate 64 0 0 0
orGate64 = orGate 64 0 0 0

notGate64, idGate64 :: CircuitBuilder (IWire, OWire)
notGate64 = notGate 64 0 0
idGate64 = idGate 64 0 0

connectWire64 :: OWire -> IWire -> CircuitBuilder ()
connectWire64 o i = connectWire (o, 64, 0) (i, 64, 0)

connectWire0_64 :: OWire -> IWire -> CircuitBuilder ()
connectWire0_64 o i = connectWire (o, 1, 0) (i, 64, 0)
