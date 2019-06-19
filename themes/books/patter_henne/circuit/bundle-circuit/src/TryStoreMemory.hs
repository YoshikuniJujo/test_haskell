{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryStoreMemory () where

import Data.Bits
import Data.Word

import Circuit
import Clock
import Memory
import TrySingleCycle
import SampleInstructions

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
