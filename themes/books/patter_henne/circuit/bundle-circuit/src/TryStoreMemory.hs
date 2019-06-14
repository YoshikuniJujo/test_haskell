{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryStoreMemory () where

import Data.Bits
import Data.Word

import Circuit
import Clock
import Memory
import TrySingleCycle

sampleStoreInstructions :: [Word64]
sampleStoreInstructions = fromIntegral . packStore <$> [
	Store (Reg 1) 8 (Reg 2),	-- sd ra,8(sp)		x1 -> x2 + 8
	Store (Reg 3) 24 (Reg 2),	-- sd gp,24(sp)		x3 -> x2 + 24
	Store (Reg 4) 32 (Reg 2),	-- sd tp,32(sp)		x4 -> x2 + 32
	Store (Reg 17) 64 (Reg 8),	-- sd x17,64(x8)	x17 -> x8 + 64
	Store (Reg 28) 16 (Reg 11) ]	-- sd x28,16(x11)	x28 -> x11 + 16

data Reg = Reg Word8 deriving Show
type Imm = Word8
data Store = Store Reg Imm Reg deriving Show

packStore :: Store -> Word32
packStore (Store (Reg rs2) imm (Reg rs1)) =
	packStype [imm `shiftR` 5, rs2, rs1, 3, imm .&. 0x1f, 35]

packStype :: [Word8] -> Word32
packStype ws = imm11_5 .|. r2 .|. r1 .|. f3 .|. imm4_0 .|. op
	where
	[imm11_5_, r2_, r1_, f3_, imm4_0_, op] = fromIntegral <$> ws
	imm11_5 = imm11_5_ `shiftL` 25
	r2 = r2_ `shiftL` 20; r1 = r1_ `shiftL` 15
	f3 = f3_ `shiftL` 12; imm4_0 = imm4_0_ `shiftL` 7

unpackStype :: Word32 -> [Word8]
unpackStype w = fromIntegral <$> [
	imm11_5_ `shiftR` 25, rs2_ `shiftR` 20, rs1_ `shiftR` 15,
	f3_ `shiftR` 12, imm4_0_ `shiftR` 7, op ]
	where
	[imm11_5_, rs2_, rs1_, f3_, imm4_0_, op] = map (w .&.) [
		0xfe000000,
		0x01f00000,
		0x000f8000,
		0x00007000,
		0x00000f80,
		0x0000007f ]

((cl, pc, rim, ig), cct) = makeCircuit tryStoreMemory

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleStoreInstructions

cct2 = resetProgramCounter pc cct1

cct3 = clockOn cl cct2
