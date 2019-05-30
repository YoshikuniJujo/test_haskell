{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.TryRun where

import Control.Monad
import Data.Bits
import Data.Word

import qualified Data.Bits as B

import Circuit
import RiscV.Memory
import Clock
import RiscV.Alu

sampleInstructions :: [Word8]
sampleInstructions = [
	0x13, 0x85, 0x87, 0xfd,
	0xb3, 0x04, 0x5a, 0x01,
	0xb3, 0x84, 0x9a, 0x00,
	0xb3, 0x05, 0x95, 0x40
	]

cutBits :: Int -> Int -> Word32 -> Word8
cutBits bg bs w = fromIntegral $ (w .&. msk) `shiftR` (bg - bs + 1)
	where
	msk = foldl B.setBit zeroBits [bg, bg - 1 .. bg - bs + 1]

separateRtype :: Word32 -> [Word8]
separateRtype w = (flip (uncurry cutBits) w)
	<$> [(31, 7), (24, 5), (19, 5), (14, 3), (11, 5), (6, 7) ]

separateToWord8 :: Word32 -> [Word8]
separateToWord8 w = (flip (uncurry cutBits) w)
	<$> reverse [(31, 8), (23, 8), (15, 8), (7, 8)]

(<-<), (>->) :: Bits a => a -> Int -> a
(<-<) = shiftL
(>->) = shiftR

packRtype :: [Word8] -> Word32
packRtype w = f7 <-< 25 .|.
	rs2 <-< 20 .|. rs1 <-< 15 .|. f3 <-< 12 .|. rd <-< 7 .|. op
	where [f7, rs2, rs1, f3, rd, op] = fromIntegral <$> w

sampleInstMemory :: Sram -> DoCircuit
sampleInstMemory sr =
	foldr (.) id $ zipWith (storeSram sr) [0 ..] sampleInstructions

countUp :: CircuitBuilder (Clock, Register)
countUp = do
	cl <- clock 50
	rg <- riscvRegister
	four <- const4
	(bpc, four', apc) <- riscvAdder
	connectWire (sigClock cl) (syncRegister rg)
	zipWithM_ connectWire (loadRegister rg) bpc
	zipWithM_ connectWire four four'
	zipWithM_ connectWire apc (storeRegister rg)
	return (cl, rg)

const4 :: CircuitBuilder [OWire]
const4 = do
	bs1 <- 2 `replicateM` constGate O
	b <- constGate I
	bs2 <- 61 `replicateM` constGate O
	return $ bs1 ++ [b] ++ bs2
