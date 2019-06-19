{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryStoreMemory () where

import Data.Bits
import Data.Word

import Circuit
import Clock
import Memory
import TrySingleCycle
import MakeInstruction

sampleStoreInstructions :: [Word64]
sampleStoreInstructions = fromIntegral . packStore <$> [
	Store (Reg 1) 8 (Reg 2),	-- sd ra,8(sp)		x1 -> x2 + 8
	Store (Reg 3) 24 (Reg 2),	-- sd gp,24(sp)		x3 -> x2 + 24
	Store (Reg 4) 32 (Reg 2),	-- sd tp,32(sp)		x4 -> x2 + 32
	Store (Reg 17) 72 (Reg 8),	-- sd x17,64(x8)	x17 -> x8 + 64
	Store (Reg 28) 16 (Reg 11) ]	-- sd x28,16(x11)	x28 -> x11 + 16

((cl, pc, rim, rrf, ig, ad, rdm), cct) = makeCircuit tryStoreMemory

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleStoreInstructions

cct2 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct1 $ zip
	[2, 8, 11] [40, 88, 104]

cct3 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct2 $ zip
	[1, 3, 4, 17, 28]
	[1234567890, 9876543210, 9999999999, 1111111111, 7777777777]

cct4 = resetProgramCounter pc cct3

cct5 = clockOn cl cct4
