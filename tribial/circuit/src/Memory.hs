{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Memory where

import Circuit
import Element

flipFlop :: CircuitBuilder (IWire, IWire, OWire, OWire)
flipFlop = do
	(r, q_', q) <- norGate
	(s, q', q_) <- norGate
	connectWire q q'
	connectWire q_ q_'
	return (r, s, q, q_)
