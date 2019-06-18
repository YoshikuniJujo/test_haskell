{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryBeq () where

import Data.Bits
import Data.Word
import Data.Int

import Circuit
import Clock
import Memory
import ImmGen
import TrySingleCycle

sampleBeqInstructions :: [Word64]
sampleBeqInstructions = fromIntegral . packSbtype . beqToWords <$> [
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

type Offset = Int16
data Reg = Reg Word8 deriving Show

data Beq = Beq Reg Reg Offset | Nop deriving Show

beqToWords :: Beq -> [Word8]
beqToWords (Beq (Reg rs1) (Reg rs2) imm) =
	[0x67, fromIntegral imm1, 0, rs1, rs2, fromIntegral imm2]
	where
	imm1 = imm .&. 0x1e .|. imm `shiftR` 11 .&. 0x01
	imm2 = imm `shiftR` 5 .&. 0x3f .|. imm `shiftR` 6 .&. 0x40
beqToWords Nop = [0x13, 0, 0, 0, 0, 0]

packSbtype :: [Word8] -> Word32
packSbtype ws@[_op, _imm1, _f_, _rs1, _rs2, _imm2] = 
	op .|. imm1 `shiftL` 7 .|. f3 `shiftL` 12 .|. rs1 `shiftL` 15 .|.
	rs2 `shiftL` 20 .|. imm2 `shiftL` 25
	where
	[op, imm1, f3, rs1, rs2, imm2] = fromIntegral <$> ws
packSbtype _ = error "Oops!"

((cl, pc, rim, ig), cct) = makeCircuit tryBeq

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleBeqInstructions

cct2 = resetProgramCounter pc cct1

cct3 = clockOn cl cct2
