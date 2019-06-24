{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySingleCycle where

import Data.Word

import Circuit
import Element
import Clock
import Memory
import Alu
import ImmGen
import Control

tryProgramCounter :: CircuitBuilder (Clock, ProgramCounter)
tryProgramCounter = do
	cl <- clock 20
	pc <- programCounter
	pcClocked cl pc
	return (cl, pc)

tryCountup :: CircuitBuilder (Clock, ProgramCounter)
tryCountup = do
	cl <- clock 30
	pc <- programCounter
	pcClocked cl pc
	ad <- riscvAdder
	connectWire64 (pcOutput pc)  (addrArgA ad)
	four <- constGate64 (Bits 4)
	connectWire64 four (addrArgB ad)
	connectWire64 (addrResult ad) (pcInput pc)
	return (cl, pc)

tryInstMem :: Word8 -> CircuitBuilder (Clock, ProgramCounter, RiscvInstMem)
tryInstMem n = do
	cl <- clock n
	pc <- programCounter
	pcClocked cl pc
	ad <- riscvAdder
	connectWire64 (pcOutput pc) (addrArgA ad)
	four <- constGate64 (Bits 4)
	connectWire64 four (addrArgB ad)
	connectWire64 (addrResult ad) (pcInput pc)
	rim <- riscvInstMem 64
	connectWire64 (pcOutput pc) (rimReadAddress rim)
	return (cl, pc, rim)

tryInstMemBranch :: CircuitBuilder (Clock, ProgramCounter, RiscvInstMem, OWire, IWire)
tryInstMemBranch = do
	cl <- clock 45
	pc <- programCounter
	pcClocked cl pc
	ad <- riscvAdder
	connectWire64 (pcOutput pc) (addrArgA ad)
	four <- constGate64 (Bits 4)
	connectWire64 four (addrArgB ad)
--	connectWire64 (addrResult ad) (pcInput pc)
	rim <- riscvInstMem 64
	connectWire64 (pcOutput pc) (rimReadAddress rim)
	return (cl, pc, rim, addrResult ad, pcInput pc)

data ReadReg = ReadReg Word8 deriving Show
data WriteReg = WriteReg Word8 deriving Show

decodeRTypeFromWords ::
	[Word8] -> (Word8, ReadReg, ReadReg, Word8, WriteReg, Word8)
decodeRTypeFromWords [f7, r2, r1, f3, rd, op] =
	(f7, ReadReg r2, ReadReg r1, f3, WriteReg rd, op)
decodeRTypeFromWords _ = error "Oops!"

tryRegisterFile :: CircuitBuilder (Clock, ProgramCounter, RiscvInstMem, RiscvRegisterFile)
tryRegisterFile = do
	(cl, pc, rim) <- tryInstMem 30
	rrf <- riscvRegisterFile
	connectWire
		(instructionMemoryOutput rim, 5, 15)
		(registerFileReadAddress1 rrf, 5, 0)
	connectWire
		(instructionMemoryOutput rim, 5, 20)
		(registerFileReadAddress2 rrf, 5, 0)
	return (cl, pc, rim, rrf)

tryRtypeAdder :: CircuitBuilder
	(Clock, ProgramCounter, RiscvInstMem, RiscvRegisterFile, OWire)
tryRtypeAdder = do
	(cl, pc, rim, rrf) <- tryRegisterFile
	ad <- riscvAdder
	connectWire64 (rrfOutput1 rrf) (addrArgA ad)
	connectWire64 (rrfOutput2 rrf) (addrArgB ad)
	connectWire0 (clockSignal cl) (rrfClock rrf)
	one <- constGate0 (Bits 1)
	connectWire0 one (rrfWrite rrf)
	connectWire
		(instructionMemoryOutput rim, 5, 7)
		(registerFileWriteAddress rrf, 5, 0)
	connectWire64 (addrResult ad) (rrfInput rrf)
	return (cl, pc, rim, rrf, addrResult ad)

tryLoadMemory :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem,
	RiscvRegisterFile, ImmGenItype, RiscvAdder, RiscvDataMem )
tryLoadMemory = do
	(cl, pc, rim) <- tryInstMem 30
	ig@(ImmGenItype igin igout) <- immGenItype
	connectWire64 (instructionMemoryOutput rim) igin
	rrf <- riscvRegisterFile
	connectWire
		(instructionMemoryOutput rim, 5, 15)
		(registerFileReadAddress1 rrf, 5, 0)
	ad <- riscvAdder
	connectWire64 (rrfOutput1 rrf) (addrArgA ad)
	connectWire64 igout (addrArgB ad)
	rdm <- riscvDataMem 64
	one <- constGate0 $ Bits 1
	connectWire0 one (rdmRead rdm)
	connectWire64 (addrResult ad) (rdmAddress rdm)

	connectWire0 (clockSignal cl) (rrfClock rrf)
	connectWire0 one (rrfWrite rrf)
	connectWire
		(instructionMemoryOutput rim, 5, 7)
		(registerFileWriteAddress rrf, 5, 0)
	connectWire64 (rdmOutput rdm) (rrfInput rrf)

	return (cl, pc, rim, rrf, ig, ad, rdm)

tryStoreMemory :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem, RiscvRegisterFile, ImmGenStype,
	RiscvAdder, RiscvDataMem )
tryStoreMemory = do
	(cl, pc, rim) <- tryInstMem 30
	ig <- immGenStype
	connectWire64 (instructionMemoryOutput rim) (igsInput ig)
	rrf <- riscvRegisterFile
	connectWire
		(instructionMemoryOutput rim, 5, 15)
		(registerFileReadAddress1 rrf, 5, 0)
	connectWire
		(instructionMemoryOutput rim, 5, 20)
		(registerFileReadAddress2 rrf, 5, 0)
	ad <- riscvAdder
	connectWire64 (rrfOutput1 rrf) (addrArgA ad)
	connectWire64 (igsOutput ig) (addrArgB ad)
	rdm <- riscvDataMem 64
	one <- constGate0 $ Bits 1
	connectWire0 one $ rdmWrite rdm
	connectWire0 (clockSignal cl) (dataMemClock rdm)
	connectWire64 (addrResult ad) (rdmAddress rdm)
	connectWire64 (rrfOutput2 rrf) (rdmInput rdm)
	return (cl, pc, rim, rrf, ig, ad, rdm)

tryBeq :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem, RiscvRegisterFile,
	RiscvSubtractor, ImmGenSbtype, RiscvAdder )
tryBeq = do
	(cl, pc, rim, npc, pcin) <- tryInstMemBranch
	ig <- immGenSbtype
	connectWire64 (instructionMemoryOutput rim) (igsbInput ig)
	rrf <- riscvRegisterFile
	connectWire
		(instructionMemoryOutput rim, 5, 15)
		(registerFileReadAddress1 rrf, 5, 0)
	connectWire
		(instructionMemoryOutput rim, 5, 20)
		(registerFileReadAddress2 rrf, 5, 0)
	sb <- riscvSubtractor
	connectWire64 (rrfOutput1 rrf) (sbrArgA sb)
	connectWire64 (rrfOutput2 rrf) (sbrArgB sb)
	ad <- riscvAdder
	connectWire64 (pcOutput pc) (addrArgA ad)
	connectWire64 (igsbOutput ig) (addrArgB ad)
	(sl, pc0, pc1, pcout) <- mux2
	connectWire0 (sbrZero sb) sl
	connectWire64 npc pc0
	connectWire64 (addrResult ad) pc1
	connectWire64 pcout pcin
	return (cl, pc, rim, rrf, sb, ig, ad)

tryControl :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem, MainController, RiscvRegisterFile,
	RiscvAlu )
tryControl = do
	(mcl, pc, rim) <- tryInstMem 100
	mctrl <- mainController
	connectWire0 (clockSignal mcl) (mainControllerExternalClockIn mctrl)
	connectWire64 (rimOutput rim) (mainControllerInstructionIn mctrl)
	(acinst, acctrl, acout) <- aluControl
	connectWire64 (rimOutput rim) acinst
	connectWire64 (mainControllerFlagsOut mctrl) acctrl
	rrf <- riscvRegisterFile
	connectWire
		(instructionMemoryOutput rim, 5, 15)
		(registerFileReadAddress1 rrf, 5, 0)
	connectWire
		(instructionMemoryOutput rim, 5, 20)
		(registerFileReadAddress2 rrf, 5, 0)
	alu <- riscvAlu
	connectWire64 acout (aluOpcode alu)
	connectWire64 (rrfOutput1 rrf) (aluArgA alu)
	(immin, immout) <- immGen
	connectWire64 (rimOutput rim) immin
	(srcSel, srcReg, srcImm, srcConn) <- mux2
	connectWire (mainControllerFlagsOut mctrl, 1, 7) (srcSel, 1, 0)
	connectWire64 (rrfOutput2 rrf) srcReg
	connectWire64 immout srcImm
	connectWire64 srcConn (aluArgB alu)
	return (mcl, pc, rim, mctrl, rrf, alu)
