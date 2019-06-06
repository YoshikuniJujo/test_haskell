{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryCircuit where

import Circuit

decode4 :: CircuitBuilder (IWire, OWire)
decode4 = do
	(iin, iout) <- idGate 2 0 0
	(rin, rout) <- idGate 4 0 0
	(ni0, no0) <- notGate 1 0 0
	(ni1, no1) <- notGate 1 0 0
	(a0, b0, o0) <- andGate 1 0 0 0
	(a1, b1, o1) <- andGate 1 0 0 0
	(a2, b2, o2) <- andGate 1 0 0 0
	(a3, b3, o3) <- andGate 1 0 0 0
	connectWire (iout, 1, 0) (ni0, 1, 0)
	connectWire (iout, 1, 1) (ni1, 1, 0)
	connectWire (no0, 1, 0) (a0, 1, 0)
	connectWire (no1, 1, 0) (b0, 1, 0)
	connectWire (iout, 1, 0) (a1, 1, 0)
	connectWire (no1, 1, 0) (b1, 1, 0)
	connectWire (no0, 1, 0) (a2, 1, 0)
	connectWire (iout, 1, 1) (b2, 1, 0)
	connectWire (iout, 1, 0) (a3, 1, 0)
	connectWire (iout, 1, 1) (b3, 1, 0)
	connectWire (o0, 1, 0) (rin, 1, 0)
	connectWire (o1, 1, 0) (rin, 1, 1)
	connectWire (o2, 1, 0) (rin, 1, 2)
	connectWire (o3, 1, 0) (rin, 1, 3)
	return (iin, rout)

-- decode3_8 :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire)
