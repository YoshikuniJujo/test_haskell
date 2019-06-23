{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ImmGen where

import Circuit
import Element


data ImmGen = ImmGen {
	igInput :: IWire, igOutput :: OWire }
	deriving Show

immGen :: CircuitBuilder (IWire, OWire)
immGen = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	connectWire (iout, 1, 31) (oin, 52, 12)
	connectWire (iout, 6, 25) (oin, 6, 5)
	(s1, is, sb, ii) <- mux2
	connectWire (iout, 1, 6) (s1, 1, 0)
	connectWire (iout, 1, 31) (is, 1, 11)
	connectWire (iout, 1, 7) (sb, 1, 11)
	connectWire (ii, 1, 11) (oin, 1, 11)
	(s2, i, ssb, fo) <- mux2
	connectWire (iout, 1, 5) (s2, 1, 0)
	connectWire (iout, 4, 21) (i, 4, 1)
	connectWire (iout, 4, 8) (ssb, 4, 1)
	connectWire (fo, 4, 1) (oin, 4, 1)
	(s3, i', s', sb', z) <- mux3
	zero <- constGate0 $ Bits 0
	connectWire (iout, 2, 5) (s3, 2, 0)
	connectWire (iout, 1, 20) (i', 1, 0)
	connectWire (iout, 1, 7) (s', 1, 0)
	connectWire (zero, 1, 0) (sb', 1, 0)
	connectWire (z, 1, 0) (oin, 1, 0)
	return (iin, oout)

data ImmGenItype = ImmGenItype {
	igiInput :: IWire, igiOutput :: OWire }
	deriving Show

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

data ImmGenStype = ImmGenStype {
	igsInput :: IWire, igsOutput :: OWire }
	deriving Show

immGenStypeGen :: CircuitBuilder (IWire, OWire)
immGenStypeGen = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	connectWire (iout, 1, 31) (oin, 53, 11)
	connectWire (iout, 6, 25) (oin, 6, 5)
	connectWire (iout, 5, 7) (oin, 5, 0)
	return (iin, oout)

immGenStype :: CircuitBuilder ImmGenStype
immGenStype = do
	(inw, outw) <- immGenStypeGen
	return $ ImmGenStype inw outw

data ImmGenSbtype = ImmGenSbtype {
	igsbInput :: IWire, igsbOutput :: OWire }
	deriving Show

immGenSbtypeGen :: CircuitBuilder (IWire, OWire)
immGenSbtypeGen = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	connectWire (iout, 1, 31) (oin, 52, 12)
	connectWire (iout, 6, 25) (oin, 6, 5)
	connectWire (iout, 4, 8) (oin, 4, 1)
	connectWire (iout, 1, 7) (oin, 1, 11)
	zero <- constGate0 $ Bits 0
	connectWire (zero, 1, 0) (oin, 1, 0)
	return (iin, oout)

immGenSbtype :: CircuitBuilder ImmGenSbtype
immGenSbtype = do
	(inw, outw) <- immGenSbtypeGen
	return $ ImmGenSbtype inw outw
