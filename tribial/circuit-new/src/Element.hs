{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Circuit

norGate :: CircuitBuilder (IWire, IWire, OWire)
norGate = do
	(i1, i2, o) <- orGate
	(ni, no) <- notGate
	connectWire o ni
	return (i1, i2, no)
