{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryRegisterFile () where

import Circuit
import Clock
import Memory
import TrySingleCycle

((cl, pc, rim, rrf), cct) = makeCircuit tryRegisterFile

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleInstructions

cct2 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct1 $ zip
	[10, 15, 30, 1, 2, 4, 9, 20, 21]
	[1234567890, 9876543210, 9999999999, 1111111111, 7777777777, 4444444444,
		1919191919, 2020202020, 2121212121]

-- 1234567890, 9876543210
-- 1111111111, 7777777777
-- 9876543210, 4444444444
-- 2020202020, 2121212121
-- 2121212121, 1919191919

cct3 = resetProgramCounter pc cct2
cct4 = clockOn cl cct3
