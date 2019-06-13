{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ImmGen where

import Circuit
-- import Element

data ImmGenItype = ImmGenItype {
	igiInput :: IWire, igiOutput :: OWire
	} deriving Show

immGenItypeGen :: CircuitBuilder (IWire, OWire)
immGenItypeGen = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	connectWire (iout, 1, 31) (oin, 53, 11)
	connectWire (iout, 11, 20) (oin, 11, 0)
	return (iin, oout)

immGenItype :: CircuitBuilder ImmGenItype
immGenItype = do
	(inw, outw) <- immGenItypeGen
	return $ ImmGenItype inw outw
