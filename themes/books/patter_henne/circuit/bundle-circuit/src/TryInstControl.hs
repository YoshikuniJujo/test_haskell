{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryInstControl () where

import Circuit
import Clock
import Memory
import Control
import TrySingleCycle
import SampleInstructions

((cl, pc, rim, mctrl), cct) = makeCircuit tryControl

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleInstControlInstructions

cct2 = resetMainController mctrl cct1

cct3 = clockOn cl cct2

cct4 = resetProgramCounter pc cct3

cct5 = stopProgramCounter pc 255 cct4
