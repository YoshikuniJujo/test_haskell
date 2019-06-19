{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryLoadMemory () where

import Data.Bits
import Data.Word

import Circuit
import Clock
import Memory
import TrySingleCycle
import Alu
import SampleInstructions

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
