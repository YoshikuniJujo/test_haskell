module RiscV.TryRunFetchInstructions () where

import Data.List
import Data.Word

import Circuit
import CircuitTools
import RiscV.Tools
import RiscV.TryRun
import Clock
import RiscV.Memory

{-
((cl, rg, sr@(_, _, _, _, os)), cct) = makeCircuit fetchInstruction32

cct01 = sampleInstMemory32 sr cct

cct02 = reset cl cct01

cct03 = resetRegister rg cct02

instruction32s :: [Word32]
instruction32s = map (bitsToNum . head) . group $ peekOWires os <$> iterate step cct03
-}
