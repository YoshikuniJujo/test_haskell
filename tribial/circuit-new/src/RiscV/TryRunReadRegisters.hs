module RiscV.TryRunReadRegisters () where

import Circuit
import CircuitTools
import Clock
import RiscV.Memory
import RiscV.TryRun

((cl, pc, im, rf, ows), cct) = makeCircuit $ readRegisters 64 32

cct1 = storeSramWithSwitch im 0 (packRtype [0, 5, 3, 0, 9, 51]) cct

cct2 = storeRegisterFile rf 5 123 cct1

cct3 = reset cl cct2

cct4 = resetRegister pc cct3

cct5 = run 30 cct4

rs1 = readRegisterFileBits rf cct1

rs2 = readRegisterFileBits rf cct2

rs3 = readRegisterFileBits rf cct3

rs4 = readRegisterFileBits rf cct4

rs5 = readRegisterFileBits rf cct5
