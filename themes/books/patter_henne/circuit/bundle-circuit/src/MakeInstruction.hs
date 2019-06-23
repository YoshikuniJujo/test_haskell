{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MakeInstruction (Reg(..), Inst(..), encodeInst) where

import Data.Bits
import Data.Word
import Data.Int

data Reg = Reg Word8 deriving Show
type Imm = Word16

data Inst
	= Load Reg Imm Reg | Store Reg Imm Reg
	| Add Reg Reg Reg | Sub Reg Reg Reg
	| Beq Reg Reg Offset | Nop
	deriving Show

encodeInst :: Inst -> Word64
encodeInst (Load rd imm rs1) = fromIntegral . packLoad $ LLoad rd imm rs1
encodeInst (Store rs2 imm rs1) = fromIntegral . packStore $ SStore rs2 imm rs1
encodeInst (Add rd rs1 rs2) = fromIntegral . packRtypeInst $ AAdd rd rs1 rs2
encodeInst (Sub rd rs1 rs2) = fromIntegral . packRtypeInst $ SSub rd rs1 rs2
encodeInst (Beq rs1 rs2 os) = fromIntegral . packSbtype . beqToWords $ BBeq rs1 rs2 os
encodeInst Nop = fromIntegral . packSbtype $ beqToWords NNop

data Rtype = AAdd Reg Reg Reg | SSub Reg Reg Reg deriving Show

packRtypeInst :: Rtype -> Word32
packRtypeInst (AAdd (Reg rd) (Reg rs1) (Reg rs2)) =
	packRType [0, rs2, rs1, 0, rd, 0b0110011]
packRtypeInst (SSub (Reg rd) (Reg rs1) (Reg rs2)) =
	packRType [0b0100000, rs2, rs1, 0, rd, 0b0110011]

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

data Load = LLoad Reg Imm Reg deriving Show

packLoad :: Load -> Word32
packLoad (LLoad (Reg rd) imm_ (Reg r1)) = packIType [imm1, imm0, r1, 3, rd, 3]
	where
	imm1 = fromIntegral $ imm_ `shiftR` 8
	imm0 = fromIntegral imm_

packIType :: [Word8] -> Word32
packIType ws = imm1 .|. imm0 .|. r1 .|. f3 .|. rd .|. op
	where
	[imm1_, imm0_, r1_, f3_, rd_, op] = fromIntegral <$> ws
	imm1 = imm1_ `shiftL` 28
	imm0 = imm0_ `shiftL` 20
	r1 = r1_ `shiftL` 15
	f3 = f3_ `shiftL` 12
	rd = rd_ `shiftL` 7

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

data Store = SStore Reg Imm Reg deriving Show

packStore :: Store -> Word32
packStore (SStore (Reg rs2) imm (Reg rs1)) =
	packStype [
		fromIntegral $ imm `shiftR` 5, rs2, rs1, 3,
		fromIntegral $ imm .&. 0x1f, 35 ]

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

type Offset = Int16

data Beq = BBeq Reg Reg Offset | NNop deriving Show

beqToWords :: Beq -> [Word8]
beqToWords (BBeq (Reg rs1) (Reg rs2) imm) =
	[0x67, fromIntegral imm1, 0, rs1, rs2, fromIntegral imm2]
	where
	imm1 = imm .&. 0x1e .|. imm `shiftR` 11 .&. 0x01
	imm2 = imm `shiftR` 5 .&. 0x3f .|. imm `shiftR` 6 .&. 0x40
beqToWords NNop = [0x13, 0, 0, 0, 0, 0]

packSbtype :: [Word8] -> Word32
packSbtype ws@[_op, _imm1, _f_, _rs1, _rs2, _imm2] = 
	op .|. imm1 `shiftL` 7 .|. f3 `shiftL` 12 .|. rs1 `shiftL` 15 .|.
	rs2 `shiftL` 20 .|. imm2 `shiftL` 25
	where
	[op, imm1, f3, rs1, rs2, imm2] = fromIntegral <$> ws
packSbtype _ = error "Oops!"
