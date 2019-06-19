{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryBeq () where

import Data.Bits
import Data.Word
import Data.Int

import Circuit
import Clock
import Memory
import ImmGen
import Alu
import TrySingleCycle
import MakeInstruction

sampleBeqInstructions :: [Word64]
sampleBeqInstructions = encodeInst <$> [
	Beq (Reg 30) (Reg 31) 20,		-- x30 == x31
	Nop,
	Nop,
	Beq (Reg 10) (Reg 11) 16,		-- x10 == x11
	Nop,
	Beq (Reg 5) (Reg 6) 28,			-- x5 /= x6
	Beq (Reg 28) (Reg 29) (- 12),		-- x28 == x29
	Beq (Reg 12) (Reg 13) (- 12) ]		-- x12 == x13

{-
beq rs1,rs2,offset
beq t5, t6, write_tohost
beq x30, x31, write_tohost
-}

((cl, pc, rim, rrf, sb, ig, ad), cct) = makeCircuit tryBeq

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleBeqInstructions

cct2 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct1 $ zip
	[30, 31, 10, 11, 5, 6, 28, 29, 12, 13] [
		1234567890, 1234567890,
		9876543210, 9876543210,
		9999999999, 1111111111,
		7777777777, 7777777777,
		3333333333, 3333333333 ]

cct3 = resetProgramCounter pc cct2

cct4 = clockOn cl cct3
