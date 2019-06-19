{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryAluAdder () where

import Data.Word

import Circuit
import Clock
import Memory
import TrySingleCycle

import SampleInstructions

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
