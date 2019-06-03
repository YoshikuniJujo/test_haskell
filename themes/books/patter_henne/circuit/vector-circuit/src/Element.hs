{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Monad
import Circuit

xorGate :: CircuitBuilder (IWire, IWire, OWire)
xorGate = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ni, no) <- notGate
	(r1, r2, r) <- andGate
	zipWithM_ connectWire
		[aout, bout, ao, no, aout, bout, oo]
		[aa, ab, ni, r1, oa, ob, r2]
	return (ain, bin, r)

nand :: CircuitBuilder (IWire, IWire, OWire)
nand = do
	(a, b, o) <- andGate
	(ni, no) <- notGate
	connectWire o ni
	return (a, b, no)
