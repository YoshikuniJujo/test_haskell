{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySingleCycle where

import Circuit
import Clock
import Memory
import Alu

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
	cl <- clock 25
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
