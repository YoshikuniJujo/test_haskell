{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ImmGen where

import Circuit
import Element

immGenItype :: CircuitBuilder (IWire, OWire)
immGenItype = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	connectWire (iout, 1, 31) (oin, 53, 11)
	connectWire (iout, 11, 20) (oin, 11, 0)
	return (iin, oout)
