{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Circuit

nandGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
nandGate ln pi1 pi2 po = do
	(a, b, o) <- andGate ln pi1 pi2 po
	(ni, no) <- notGate ln po po
	connectWire (o, ln, po) (ni, ln, po)
	return (a, b, no)
