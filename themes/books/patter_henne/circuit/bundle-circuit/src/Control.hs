{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control (
	MainController, mainController, resetMainController,
	control, microControl ) where

import Circuit
import Element
import Clock
import Memory
import MicroClock

data MainController = MainController {
	mcMicroClock :: Clock, mcMicroClockFlag :: Register,
	mcExternalClockIn :: IWire,
	mcState :: Register, mcInstIn :: IWire, mcFlagsOut :: OWire }
	deriving Show

mainController :: CircuitBuilder MainController
mainController = do
	(mcl, mclf, eci, st, inst, fo) <- microControl
	return $ MainController mcl mclf eci st inst fo

resetMainController :: MainController -> Circuit -> Circuit
resetMainController mc cct = let
	cct0 = resetRegister (mcMicroClockFlag mc) cct
	cct1 = resetRegister (mcState mc) cct0
	cct2 = clockOn (mcMicroClock mc) cct1 in
	cct2

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

microControl :: CircuitBuilder (Clock, Register, IWire, Register, IWire, OWire)
microControl = do
	(st, inst, ne, out) <- control
	(mc, mcf, ec) <- microClocked 18 (registerClock st) ne
	return (mc, mcf, ec, st, inst, out)
