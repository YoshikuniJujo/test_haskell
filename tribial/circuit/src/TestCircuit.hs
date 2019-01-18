{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TestCircuit where

import Control.Arrow

import Circuit

--------------------------------------------------------------------------------

mux2 :: CircuitBuilder (InWire, InWire, InWire, OutWire)
mux2 = do
	(a, s1, a1o) <- andGate
	(b, s2, a2o) <- andGate
	(xi1, xi2, c) <- orGate
	(ni, no) <- notGate
	(si, so) <- idGate
	connectWire so ni
	connectWire no s1
	connectWire so s2
	connectWire a1o xi1
	connectWire a2o xi2
	return (a, b, si, c)

type SBit = (String, Bit)

getMux2Wires :: (InWire, InWire, InWire, OutWire) ->
	Circuit -> ((SBit, SBit, SBit), SBit)
getMux2Wires (a, b, s, c) =
	(\[x, y, z] -> (x, y, z)) . zip ["a", "b", "s"]
		. (\ctt -> map (`peekIWire` ctt) [a, b, s]) &&&
	(("c" ,) . peekOWire c)

setMux2Wires ::
	(InWire, InWire, InWire, a) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setMux2Wires (a, b, s, _) (ba, bb, bs) = setBit a ba . setBit b bb . setBit s bs
