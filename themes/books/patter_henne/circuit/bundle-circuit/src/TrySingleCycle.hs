{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySingleCycle where

import Data.Bits
import Data.Word

import Circuit
import Clock
import Memory
import Alu
import ImmGen

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

tryInstMem :: CircuitBuilder (Clock, ProgramCounter, RiscvInstMem)
tryInstMem = do
	cl <- clock 30
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

sampleInstructions :: [Word64]
sampleInstructions = fromIntegral <$> [
	0x00f507b3,		-- add a5, a0, a5	x15, x10, x15
	0x40208f33,		-- sub r30, r01, r02	x30, x1 , x2
	0x004787b3,		-- add a5, a5, tp	x15, x15, x4
	0x015a04b3,		-- add x9, x20, x21	x9 , x20, x21
	0x009a84b3 :: Word32	-- add x9, x21, x9	x9 , x21, x9
	]

-- R type: 7 5 5 3 5 7

packRType :: [Word8] -> Word32
packRType ws = f7 .|. r2 .|. r1 .|. f3 .|. rd .|. op
	where
	[f7_, r2_, r1_, f3_, rd_, op] = fromIntegral <$> ws
	f7 = f7_ `shiftL` 25; r2 = r2_ `shiftL` 20; r1 = r1_ `shiftL` 15
	f3 = f3_ `shiftL` 12; rd = rd_ `shiftL` 7

unpackRType :: Word64 -> [Word8]
unpackRType w = fromIntegral <$> [
	f7_ `shiftR` 25, r2_ `shiftR` 20, r1_ `shiftR` 15,
	f3_ `shiftR` 12, rd_ `shiftR` 7, op ]
	where
	[f7_, r2_, r1_, f3_, rd_, op] = map (w .&.) [
		0xfe000000,
		0x01f00000,
		0x000f8000,
		0x00007000,
		0x00000f80,
		0x0000007f ]

data ReadReg = ReadReg Word8 deriving Show
data WriteReg = WriteReg Word8 deriving Show

decodeRTypeFromWords ::
	[Word8] -> (Word8, ReadReg, ReadReg, Word8, WriteReg, Word8)
decodeRTypeFromWords [f7, r2, r1, f3, rd, op] =
	(f7, ReadReg r2, ReadReg r1, f3, WriteReg rd, op)
decodeRTypeFromWords _ = error "Oops!"

tryRegisterFile :: CircuitBuilder (Clock, ProgramCounter, RiscvInstMem, RiscvRegisterFile)
tryRegisterFile = do
	(cl, pc, rim) <- tryInstMem
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

tryLoadMemory :: CircuitBuilder
	(Clock, ProgramCounter, RiscvInstMem, RiscvRegisterFile, ImmGenItype)
tryLoadMemory = do
	(cl, pc, rim) <- tryInstMem
	ig@(ImmGenItype igin _igout) <- immGenItype
	connectWire64 (instructionMemoryOutput rim) igin
	rrf <- riscvRegisterFile
	connectWire
		(instructionMemoryOutput rim, 5, 15)
		(registerFileReadAddress1 rrf, 5, 0)
	return (cl, pc, rim, rrf, ig)
