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

mux2_1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2_1 = do
	(si, sout) <- idGate0
	(ni, no) <- notGate0
	(aa1, ab1, ao1) <- andGate0
	(aa2, ab2, ao2) <- andGate0
	(oa, ob, oo) <- orGate0
	connectWire0 sout ni
	connectWire0 no aa1
	connectWire0 sout aa2
	connectWire0 ao1 oa
	connectWire0 ao2 ob
	return (si, ab1, ab2, oo)

mux2_64 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2_64 = do
	(si, sout) <- idGate0
	(ni, no) <- notGate0
	connectWire0 sout ni
	(aa1, ab1, ao1) <- andGate64
	(aa2, ab2, ao2) <- andGate64
	(oa, ob, oo) <- orGate64
	connectWire0_64 no aa1
	connectWire0_64 sout aa2
	connectWire64 ao1 oa
	connectWire64 ao2 ob
	return (si, ab1, ab2, oo)
