module TryFetchInstruction (tryFetchInstructionTryRun) where

import Circuit
import Clock
import Memory
import TrySingleCycle

((cl, pc, rim), cct) = makeCircuit tryInstMem

cct1 = storeRiscvInstMem rim 0 1234567890 cct
cct2 = storeRiscvInstMem rim 4 9876543210 cct1
cct3 = storeRiscvInstMem rim 8 9999999999 cct2
cct4 = storeRiscvInstMem rim 12 1111111111 cct3
cct5 = storeRiscvInstMem rim 16 7777777777 cct4
cct6 = resetProgramCounter pc cct5
cct7 = clockOn cl cct6

middle = readRiscvInstMem rim cct4

tryRun = readRiscvInstMem rim <$> iterate step cct7

tryFetchInstructionTryRun = tryRun
