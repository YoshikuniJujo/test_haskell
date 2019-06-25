{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryFetchInstruction (tryFetchInstructionTryRun) where

import Circuit
import Clock
import Memory
import TrySingleCycle

((cl, pc, rim), cct) = makeCircuit $ do
	(cl', pc', rim', npc, pcin) <- tryInstMem 30
	connectWire64 npc pcin
	return (cl', pc', rim')

cct1 = storeRiscvInstMem rim 0 0x00f507b3 cct
cct2 = storeRiscvInstMem rim 4 0x40208f33 cct1
cct3 = storeRiscvInstMem rim 8 0x004787b3 cct2
cct4 = storeRiscvInstMem rim 12 0x015a04b3 cct3
cct5 = storeRiscvInstMem rim 16 0x009a84b3 cct4
cct6 = resetProgramCounter pc cct5
cct7 = clockOn cl cct6

middle = readRiscvInstMem rim cct4

tryRun = readRiscvInstMem rim <$> iterate step cct7

tryFetchInstructionTryRun = tryRun
