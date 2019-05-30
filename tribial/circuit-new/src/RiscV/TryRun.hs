{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.TryRun where

import Control.Monad
import Data.List
import Data.Bits hiding (setBit)
import Data.Word
import Data.Int

import qualified Data.Bits as B

import Circuit
import RiscV.Memory
import Clock
import RiscV.Alu
import RiscV.Element
import RiscV.Tools
import CircuitTools

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

sampleInstMemory1 :: Sram -> DoCircuit
sampleInstMemory1 sr = storeSram sr 0 $ head sampleInstructions

sampleInstMemory2 :: Sram -> DoCircuit
sampleInstMemory2 sr =
	foldr (.) id $ zipWith (storeSram sr) [0, 1] sampleInstructions

countUp :: CircuitBuilder (Clock, Register)
countUp = do
	cl <- clock 100
	rg <- riscvRegister
	four <- const1
--	four <- const4
--	four <- const0
	(bpc, four', apc) <- riscvAdder
	connectWire (clockOutput cl) (registerClock rg)
	zipWithM_ connectWire (registerOutput rg) bpc
	zipWithM_ connectWire four four'
	zipWithM_ connectWire apc (registerInput rg)
	return (cl, rg)

const0 :: CircuitBuilder [OWire]
const0 = 64 `replicateM` constGate O

const1 :: CircuitBuilder [OWire]
const1 = do
	b <- constGate I
	bs <- 63 `replicateM` constGate O
	return $ b : bs

const4 :: CircuitBuilder [OWire]
const4 = do
	bs1 <- 2 `replicateM` constGate O
	b <- constGate I
	bs2 <- 61 `replicateM` constGate O
	return $ bs1 ++ [b] ++ bs2

fetchInstruction :: CircuitBuilder (Clock, Register, (IWire, IWire, [IWire], [IWire], [OWire]))
fetchInstruction = do
	(cl, rg) <- countUp
	sr <- riscvSram
	(slin, sout) <- idGate
	(ss, ras, was, as) <- unzip4 <$> 64 `replicateM` mux2
	connectWire sout `mapM_` ss
	zipWithM_ connectWire (registerOutput rg) ras
	zipWithM_ connectWire as (sramAddress sr)
--	zero <- 64 `replicateM` constGate O
--	zipWithM_ connectWire zero (sramAddress sr)
	return (cl, rg, (slin, sramClock sr, was, sramInput sr, sramOutput sr))

storeSram2 :: (IWire, IWire, [IWire], [IWire], [OWire]) -> Int64 -> Word8 -> DoCircuit
storeSram2 (wr, cl, wad, wb, _) ad b = setBit wr O . run 2
	. setBit cl O . setBits wad (numToBits 64 ad) . setBits wb (numToBits 8 b) . run 5
	. setBit cl I . setBits wad (numToBits 64 ad) . setBits wb (numToBits 8 b) . run 8
	. setBit cl O . setBits wad (numToBits 64 ad) . setBits wb (numToBits 8 b) 
	. setBit wr I

loadSram2 :: (IWire, IWire, [IWire], [IWire], [OWire]) -> Int64 -> DoCircuit
loadSram2 (wr, cl, wad, _, _) ad = setBit wr I . run 4 . setBit cl O . setBits wad (numToBits 64 ad)

peekOWires :: [OWire] -> Circuit -> [Bit]
peekOWires os cct = (`peekOWire` cct) <$> os

testSram :: CircuitBuilder Sram
testSram = do
	zero <- 64 `replicateM` constGate O
	sr <- riscvSram
	zipWithM_ connectWire zero (sramAddress sr)
	return sr
