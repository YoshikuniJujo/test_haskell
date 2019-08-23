{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Monad

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

adder1bit :: CircuitBuilder Wire32
adder1bit = do
	(ciin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(sci, sa, sb, so) <- sum1bit
	(cci, ca, cb, co) <- carry1bit
	zipWithM_ connectWire64 [ciout, aout, bout] [sci, sa, sb]
	zipWithM_ connectWire64 [ciout, aout, bout] [cci, ca, cb]
	return (ciin, ain, bin, so, co)

sum1bit :: CircuitBuilder Wire31
sum1bit = xorGate3

carry1bit :: CircuitBuilder Wire31
carry1bit = do
	(ciin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa1, ab1, ao1) <- andGate
	(aa2, ab2, ao2) <- andGate
	(aa3, ab3, ao3) <- andGate
	(oa, ob, oc, co) <- orGate3
	connectWire64 ciout `mapM_` [aa1, aa2]
	connectWire64 aout `mapM_` [ab1, aa3]
	connectWire64 bout `mapM_` [ab2, ab3]
	zipWithM_ connectWire64 [ao1, ao2, ao3] [oa, ob, oc]
	return (ciin, ain, bin, co)
