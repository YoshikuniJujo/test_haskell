{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SampleInstructions where

import Data.Word

import MakeInstruction

sampleLoadInstructions :: [Word64]
sampleLoadInstructions = encodeInst <$> [
	Load (Reg 10) 0 (Reg 15),			-- x15: 8	8	1234567890
	Load (Reg 10) 56 (Reg 15),			-- x15: 8	64	9876543210
	Load (Reg 3) 16 (Reg 7),			-- x7 : 32	48	9999999999
	Load (Reg 2) 24 (Reg 4),			-- x4 : 16	40	1111111111
	Load (Reg 18) 8 (Reg 21) ]			-- x21: 24	32	7777777777

sampleStoreInstructions :: [Word64]
sampleStoreInstructions = encodeInst <$> [
	Store (Reg 1) 8 (Reg 2),	-- sd ra,8(sp)		x1 -> x2 + 8
	Store (Reg 3) 24 (Reg 2),	-- sd gp,24(sp)		x3 -> x2 + 24
	Store (Reg 4) 32 (Reg 2),	-- sd tp,32(sp)		x4 -> x2 + 32
	Store (Reg 17) 72 (Reg 8),	-- sd x17,64(x8)	x17 -> x8 + 64
	Store (Reg 28) 16 (Reg 11) ]	-- sd x28,16(x11)	x28 -> x11 + 16

sampleRtypeInstructions :: [Word64]
sampleRtypeInstructions = encodeInst <$> [
	Add (Reg 15) (Reg 10) (Reg 15), -- add a5, a0, a5	x15, x10, x15
	Sub (Reg 30) (Reg 1) (Reg 2),	-- sub r30, r01, r02	x30, x1, x2
	Add (Reg 15) (Reg 15) (Reg 4),	-- add a5, a5, tp	x15, x15, x4
	Add (Reg 9) (Reg 20) (Reg 21),	-- add x9, x20, x21	x9, x20, x21
	Add (Reg 9) (Reg 21) (Reg 9) ]	-- add x9, x21, x9	x9, x21, x9

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

sampleLoadInst, sampleStoreInst, sampleSubInst, sampleAddInst, sampleBeqInst :: Word64
sampleLoadInst = encodeInst $ Load (Reg 10) 56 (Reg 15)
sampleStoreInst = encodeInst $ Store (Reg 1) 8 (Reg 2)
sampleSubInst = encodeInst $ Sub (Reg 30) (Reg 1) (Reg 2)
sampleAddInst = encodeInst $ Add (Reg 15) (Reg 10) (Reg 15)
sampleBeqInst = encodeInst $ Beq (Reg 30) (Reg 31) 20

sampleInstControlInstructions :: [Word64]
sampleInstControlInstructions = [
	sampleLoadInst, sampleStoreInst,
	sampleSubInst, sampleAddInst, sampleBeqInst ] ++ (encodeInst <$> [
	Nop, Nop, Nop, Nop, Add (Reg 3) (Reg 1) (Reg 10) ])
