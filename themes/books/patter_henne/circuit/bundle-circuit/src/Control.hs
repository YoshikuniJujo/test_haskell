{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control where

import Data.Word

import Circuit
import Element
import MakeInstruction

dispatch1 :: CircuitBuilder (IWire, OWire)
dispatch1 = do
	(instin, instout) <- idGate64
	(nstin, nstout) <- idGate64
	b0 <- constGate0 $ Bits 0
	connectWire (b0, 1, 0) (nstin, 1, 0)
	(ni, no) <- notGate0
	connectWire (instout, 1, 6) (ni, 1, 0)
	connectWire (no, 1, 0) (nstin, 1, 1)
	connectWire (instout, 1, 4) (nstin, 1, 2)
	connectWire (instout, 1, 6) (nstin, 1, 3)
	return (instin, nstout)

dispatch2 :: CircuitBuilder (IWire, OWire)
dispatch2 = do
	(instin, instout) <- idGate64
	(nstin, nstout) <- idGate64
	b0 <- constGate0 $ Bits 1
	connectWire (b0, 1, 0) (nstin, 1, 0)
	(ni, no) <- notGate0
	connectWire (instout, 1, 5) (ni, 1, 0)
	connectWire (no, 1, 0) (nstin, 1, 1)
	connectWire (instout, 1, 5) (nstin, 1, 2)
	b3 <- constGate0 $ Bits 0
	connectWire (b3, 1, 0) (nstin, 1, 3)
	return (instin, nstout)

sampleLoadInst, sampleStoreInst, sampleAddInst, sampleBeqInst :: Word64
sampleLoadInst = encodeInst $ Load (Reg 10) 56 (Reg 15)
sampleStoreInst = encodeInst $ Store (Reg 1) 8 (Reg 2)
sampleAddInst = encodeInst $ Add (Reg 15) (Reg 10) (Reg 15)
sampleBeqInst = encodeInst $ Beq (Reg 30) (Reg 31) 20

inc :: Word8 -> OWire -> IWire -> CircuitBuilder OWire
inc 0 o i = do
	(ni, no) <- notGate0
	connectWire0 o ni
	connectWire0 no i
	(ci, co) <- idGate0
	connectWire0 o ci
	return co
inc n o i = do
	c <- inc (n - 1) o i
	(xa, xb, xo) <- xorGate0
	(aa, ab, ao) <- andGate0
	connectWire0 c xa
	connectWire0 c aa
	connectWire (o, 1, n) (xb, 1, 0)
	connectWire (o, 1, n) (ab, 1, 0)
	connectWire (xo, 1, 0) (i, 1, n)
	return ao

inc8 :: CircuitBuilder (IWire, OWire)
inc8 = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	_ <- inc 7 iout oin
	return (iin, oout)
