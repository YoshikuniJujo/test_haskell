{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Memory where

import Control.Monad

import Circuit
import Element
import CircuitTools

rslatch :: CircuitBuilder Wires22
rslatch = do
	(r, q_', q) <- norGate
	(s, q', q_) <- norGate
	zipWithM_ connectWire [q, q_] [q', q_']
	return (r, s, q, q_)

dlatch :: CircuitBuilder Wires22
dlatch = do
	(cin, cout) <- idGate
	(din, dout) <- idGate
	(ni, no) <- notGate
	(ra1, ra2, r) <- andGate
	(sa1, sa2, s) <- andGate
	(r', s', q, q_) <- rslatch
	zipWithM_ connectWire
		[dout, cout, no, cout, dout, r, s]
		[ni, ra1, ra2, sa1, sa2, r', s']
	return (cin, din, q, q_)
