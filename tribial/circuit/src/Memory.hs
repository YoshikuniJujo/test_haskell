{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Memory where

import Control.Monad
import Data.Word

import Circuit
import Element
import Clock
import Tools

crossCoupled :: CircuitBuilder (IWire, IWire, OWire, OWire)
crossCoupled = do
	(r, q_', q) <- norGate
	(s, q', q_) <- norGate
	connectWire q q'
	connectWire q_ q_'
	return (r, s, q, q_)

dlatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
dlatch = do
	((cin, cout), (din, dout)) <- listToTuple2 <$> replicateM 2 idGate
	(ni, no) <- notGate
	(c1, nd, r) <- andGate
	(c2, d, s) <- andGate
	(r', q_', q) <- norGate
	(s', q', q_) <- norGate
	zipWithM_ connectWire
		[dout, cout, no, r, cout, dout, s, q_, q]
		[ni, c1, nd, r', c2, d, s', q_', q']
	return (cin, din, q, q_)

temporary :: Word8 -> Word8 -> CircuitBuilder OWire
temporary w dr = do
	(_, on) <- notGate
	(i1, i2, o) <- xorGate
	(wi, wo) <- delay w
	(dri, dro) <- delay $ w + dr
	zipWithM_ connectWire [on, on, wo, dro] [wi, dri, i1, i2]
	return o

testDlatch :: CircuitBuilder (OWire, OWire)
testDlatch = do
	c <- edge 10 100
	d <- temporary 1 30
	(dli, dlo) <- delay 5
	(c', d', q, q_) <- dlatch
	connectWire c dli
	connectWire dlo c'
	connectWire d d'
	return (q, q_)
