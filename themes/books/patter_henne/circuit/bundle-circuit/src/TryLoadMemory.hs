{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryLoadMemory () where

import Data.Bits
import Data.Word

import Circuit
import Clock
import Memory
import TrySingleCycle
import Alu

sampleLoadInstructions :: [Word64]
sampleLoadInstructions = fromIntegral . packLoad <$> [
	Load (Reg 10) 0 (Reg 15),			-- x15: 8	8	1234567890
	Load (Reg 10) 56 (Reg 15),			-- x15: 8	64	9876543210
	Load (Reg 3) 16 (Reg 7),			-- x7 : 32	48	9999999999
	Load (Reg 2) 24 (Reg 4),			-- x4 : 16	40	1111111111
	Load (Reg 18) 8 (Reg 21) ]			-- x21: 24	32	7777777777

data Reg = Reg Word8 deriving Show
type Imm = Word8
data Load = Load Reg Imm Reg deriving Show

packLoad :: Load -> Word32
packLoad (Load (Reg rd) imm (Reg r1)) = packIType [imm, r1, 3, rd, 3]

packIType :: [Word8] -> Word32
packIType ws = imm .|. r1 .|. f3 .|. rd .|. op
	where
	[imm_, r1_, f3_, rd_, op] = fromIntegral <$> ws
	imm = imm_ `shiftL` 20; r1 = r1_ `shiftL` 15
	f3 = f3_ `shiftL` 12; rd = rd_ `shiftL` 7

unpackItype :: Word32 -> [Word8]
unpackItype w = fromIntegral <$> [
	imm_ `shiftR` 20, r1_ `shiftR` 15,
	f3_ `shiftR` 12, rd_ `shiftR` 7, op ]
	where
	[imm_, r1_, f3_, rd_, op] = map (w .&.) [
		0xfff00000,
		0x000f8000,
		0x00007000,
		0x00000f80,
		0x0000007f ]

((cl, pc, rim, rrf, igi, ad, rdm), cct) = makeCircuit tryLoadMemory

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleLoadInstructions

cct2 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct1 $ zip
	[15, 7, 4, 21] [8, 32, 16, 24]

cct3 = foldr (uncurry $ storeRiscvDataMem rdm) cct2 $ zip
	[8, 64, 48, 40, 32]
	[1234567890, 9876543210,  9999999999, 1111111111, 7777777777]

cct4 = resetProgramCounter pc cct3

cct5 = clockOn cl cct4
