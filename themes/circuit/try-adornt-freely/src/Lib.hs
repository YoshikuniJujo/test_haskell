{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Circuit.Adornt
-- import Circuit.Adornt.Samples
import Circuit.Adornt.Parts

samplePla1 :: CircuitBuilder Wire11
samplePla1 = pla8 [
	(0, 0), (1, 1), (2, 1), (3, 1), (4, 1), (5, 0), (6, 0), (7, 1) ]

samplePla2 :: CircuitBuilder Wire11
samplePla2 = pla8 [
	(0, 0), (1, 4), (2, 4), (3, 6), (4, 4), (5, 6), (6, 6), (7, 5) ]

alu0 :: CircuitBuilder Wire31
alu0 = do
	(ain, aout)  <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(op, i0, i1, o) <- mux2
	connectWire64 aout `mapM_` [aa, oa]
	connectWire64 bout `mapM_` [ab, ob]
	connectWire64 ao i0
	connectWire64 oo i1
	return (op, ain, bin, o)
