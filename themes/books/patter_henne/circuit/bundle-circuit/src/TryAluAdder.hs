{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryAluAdder () where

import Data.Word

import Circuit
import Clock
import Memory
import TrySingleCycle

import MakeInstruction

sampleRtypeInstructions :: [Word64]
sampleRtypeInstructions = encodeInst <$> [
	Add (Reg 15) (Reg 10) (Reg 15), -- add a5, a0, a5	x15, x10, x15
	Sub (Reg 30) (Reg 1) (Reg 2),	-- sub r30, r01, r02	x30, x1, x2
	Add (Reg 15) (Reg 15) (Reg 4),	-- add a5, a5, tp	x15, x15, x4
	Add (Reg 9) (Reg 20) (Reg 21),	-- add x9, x20, x21	x9, x20, x21
	Add (Reg 9) (Reg 21) (Reg 9) ]	-- add x9, x21, x9	x9, x21, x9

((cl, pc, rim, rrf, o), cct) = makeCircuit tryRtypeAdder

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleRtypeInstructions

cct2 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct1 $ zip
	[10, 15, 30, 1, 2, 4, 9, 20, 21]
	[1234567890, 9876543210, 9999999999, 1111111111, 7777777777, 4444444444,
		1919191919, 2020202020, 2121212121]

-- x10: 1234567890, x15: 9876543210 x15
-- x1 : 1111111111, x2 : 7777777777 x30	
-- x15: 9876543210, x4 : 4444444444 x15		x15: 11111111100, x4 : 4444444444
-- x20: 2020202020, x21: 2121212121 x9
-- x21: 2121212121, x9 : 1919191919 x9		x21: 2121212121 , x9 : 4141414141

cct3 = resetProgramCounter pc cct2
cct4 = clockOn cl cct3
