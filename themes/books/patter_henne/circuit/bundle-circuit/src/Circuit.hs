{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBits, peekOWire, bitsToWord,
	CircuitBuilder, connectWire, andGate, orGate, idGate, notGate,
	IWire, OWire, Bits, BitLen, BitPosIn, BitPosOut,
	andGate0, orGate0, notGate0, connectWire0
	) where

import CircuitCore

andGate0, orGate0 :: CircuitBuilder (IWire, IWire, OWire)
andGate0 = andGate 1 0 0 0
orGate0 = orGate 1 0 0 0

notGate0 :: CircuitBuilder (IWire, OWire)
notGate0 = notGate 1 0 0

connectWire0 :: OWire -> IWire -> CircuitBuilder ()
connectWire0 o i = connectWire (o, 1, 0) (i, 1, 0)
