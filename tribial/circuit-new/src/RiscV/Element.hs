{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.Element where

import Control.Monad

import Circuit

norGate :: CircuitBuilder (IWire, IWire, OWire)
norGate = do
	(i1, i2, o) <- orGate
	(ni, no) <- notGate
	connectWire o ni
	return (i1, i2, no)

type Wires31 = (IWire, IWire, IWire, OWire)

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
