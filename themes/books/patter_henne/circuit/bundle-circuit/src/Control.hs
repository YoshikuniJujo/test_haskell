{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control where

import Data.Word

import Circuit
import Element
import Clock
import Memory
import MicroClock
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

microMemory :: CircuitBuilder (IWire, OWire)
microMemory = pla8_16 $ zip [0 ..] [
	0b0000000011,
	0b0000000001,
	0b1000000010,
	0b1101000011,
	0b1111000000,
	0b1000100000,
	0b0000001011,
	0b0010001000,
	0b0000010100 ]

checkStop :: CircuitBuilder (IWire, OWire)
checkStop = pla8 $ zip [0 ..] [1, 1, 1, 1, 0, 0, 1, 0, 0]

control :: CircuitBuilder (Register, IWire, OWire, OWire)
control = do
	(instin, instout) <- idGate64
	(acin, acout) <- microMemory
	zero <- constGate64 $ Bits 0
	(d1in, d1out) <- dispatch1
	connectWire64 instout d1in
	(d2in, d2out) <- dispatch2
	connectWire64 instout d2in
	(incin, incout) <- inc8
	(sl, i0, i1, i2, i3, mo) <- mux4
	connectWire (acout, 2, 0) (sl, 2, 0)
	connectWire64 zero i0
	connectWire64 d1out i1
	connectWire64 d2out i2
	connectWire64 incout i3
	st <- register
	connectWire64 mo (registerInput st)
	connectWire64 (registerOutput st) incin
	connectWire64 (registerOutput st) acin
	(csi, cso) <- checkStop
	connectWire64 mo csi
	(oin, oout) <- idGate64
	connectWire (acout, 8, 2) (oin, 8, 0)
	return (st, instin, cso, oout)

tryControl :: CircuitBuilder (Clock, Register, IWire)
tryControl = do
	cl <- clock 15
	(r, inst, _, _) <- control
	connectWire0 (clockSignal cl) (registerClock r)
	return (cl, r, inst)

sampleLoadInst, sampleStoreInst, sampleAddInst, sampleBeqInst :: Word64
sampleLoadInst = encodeInst $ Load (Reg 10) 56 (Reg 15)
sampleStoreInst = encodeInst $ Store (Reg 1) 8 (Reg 2)
sampleAddInst = encodeInst $ Add (Reg 15) (Reg 10) (Reg 15)
sampleBeqInst = encodeInst $ Beq (Reg 30) (Reg 31) 20

cctTryControl :: Word64 -> (OWire, Circuit)
cctTryControl si = let
	((cl, rg, inst), cct) = makeCircuit tryControl
	cct0 = setBits inst (Bits si) cct
	cct1 = resetRegister rg cct0
	cct2 = clockOn cl cct1 in
	(registerOutput rg, cct2)

microControl :: CircuitBuilder (Clock, Register, IWire, Register, IWire, OWire)
microControl = do
	(st, inst, ne, out) <- control
	(mc, mcf, ec) <- microClocked 18 (registerClock st) ne
	return (mc, mcf, ec, st, inst, out)

tryMicroControl :: CircuitBuilder (Clock, Clock, Register, Register, IWire, OWire)
tryMicroControl = do
	ecl <- clock 240
	(mc, mcf, ecin, st, inst, out) <- microControl
	connectWire0 (clockSignal ecl) ecin
	return (ecl, mc, mcf, st, inst, out)

cctMicroControl :: Word64 -> (OWire, Circuit)
cctMicroControl si = let
	((ecl, mcl, mcf, st, inst, out), cct) = makeCircuit tryMicroControl
	cct0 = setBits inst (Bits si) cct
	cct1 = resetRegister mcf cct0
	cct2 = resetRegister st cct1
	cct3 = clockOn mcl cct2
	cct4 = clockOn ecl cct3 in
	(out, cct4)
