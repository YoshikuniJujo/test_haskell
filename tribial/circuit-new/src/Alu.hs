{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Alu where

import Control.Monad

import Circuit
import Element
import CircuitTools

mux2 :: CircuitBuilder Wires31
mux2 = do
	(slin, slout) <- idGate
	(ni, no) <- notGate
	(ns, a, ao) <- andGate
	(s, b, bo) <- andGate
	(ai, bi, c) <- orGate
	zipWithM_ connectWire
		[slout, no, slout, ao, bo]
		[ni, ns, s, ai, bi]
	return (slin, a, b, c)

alu1_ao :: CircuitBuilder Wires31
alu1_ao = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(s, a, o, r) <- mux2
	zipWithM_ connectWire
		[aout, bout, aout, bout, ao, oo]
		[aa, ab, oa, ob, a, o]
	return (s, ain, bin, r)

carry1 :: CircuitBuilder Wires31
carry1 = do
	(ciin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(ci1, a1, ad1) <- andGate
	(a2, b2, ad2) <- andGate
	(b3, ci3, ad3) <- andGate
	((o1, o2, o3), oo) <- orGate3
	zipWithM_ connectWire
		[ciout, aout, aout, bout, bout, ciout, ad1, ad2, ad3]
		[ci1, a1, a2, b2, b3, ci3, o1, o2, o3]
	return (ciin, ain, bin, oo)

sum1 :: CircuitBuilder Wires31
sum1 = do
	(a, b, abo) <- xorGate
	(abi, ci, r) <- xorGate
	connectWire abo abi
	return (ci, a, b, r)

adder1 :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
adder1 = do
	(ciin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(c1, a1, b1, s) <- sum1
	(c2, a2, b2, co) <- carry1
	zipWithM_ connectWire
		[ciout, aout, bout, ciout, aout, bout] [c1, a1, b1, c2, a2, b2]
	return (ciin, ain, bin, s, co)
