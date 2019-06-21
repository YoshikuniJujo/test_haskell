{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control (
	MainController, mainController, resetMainController,
	mainControllerExternalClockIn, mainControllerInstructionIn,
	mainControllerFlagsOut,
	control, microControl, aluControl ) where

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

mainControllerExternalClockIn :: MainController -> IWire
mainControllerExternalClockIn = mcExternalClockIn

mainControllerInstructionIn :: MainController -> IWire
mainControllerInstructionIn = mcInstIn

mainControllerFlagsOut :: MainController -> OWire
mainControllerFlagsOut = mcFlagsOut

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

aluControlGen :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire, OWire, OWire)
aluControlGen = do
	(op1in, op1out) <- idGate0
	(op0, op1, mem) <- orGate0
	connectWire0 op1out op1
	(mem', nmem) <- notGate0
	connectWire0 mem mem'
	(op1', nop1) <- notGate0
	connectWire0 op1out op1'
	(inst13in, inst13out) <- idGate0
	(inst12, inst13, instXor) <- xorGate0
	connectWire0 inst13out inst13
	(inst13', ninst13) <- notGate0
	connectWire0 inst13out inst13'
	(op1'', inst30, instSub) <- orGate0
	connectWire0 op1out op1''
	((r0a1, r0a2, r0a3), r0) <- andGate0_3
	connectWire0 mem r0a1
	connectWire0 nop1 r0a2
	connectWire0 instXor r0a3
	((r1o1, r1o2, r1o3), r1) <- orGate0_3
	connectWire0 nmem r1o1
	connectWire0 op1out r1o2
	connectWire0 ninst13 r1o3
	(r2a1, r2a2, r2) <- andGate0
	connectWire0 mem r2a1
	connectWire0 instSub r2a2
	return (op0, op1in, inst12, inst13in, inst30, r0, r1, r2)

aluControl :: CircuitBuilder (IWire, IWire, OWire)
aluControl = do
	(instin, instout) <- idGate64
	(mctrlin, mctrlout) <- idGate64
	(op0, op1, inst12, inst13, inst30, r0, r1, r2) <- aluControlGen
	connectWire (mctrlout, 1, 0) (op1, 1, 0)
	connectWire (mctrlout, 1, 1) (op0, 1, 0)
	connectWire (instout, 1, 12) (inst12, 1, 0)
	connectWire (instout, 1, 13) (inst13, 1, 0)
	connectWire (instout, 1, 30) (inst30, 1, 0)
	zero <- constGate0 $ Bits 0
	(rsltin, rsltout) <- idGate64
	connectWire (r0, 1, 0) (rsltin, 1, 0)
	connectWire (r1, 1, 0) (rsltin, 1, 1)
	connectWire (r2, 1, 0) (rsltin, 1, 2)
	connectWire (zero, 1, 0) (rsltin, 1, 3)
	return (instin, mctrlin, rsltout)
