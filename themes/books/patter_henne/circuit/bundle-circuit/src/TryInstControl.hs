{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryInstControl () where

import Circuit
import Clock
import Memory
import Control
import TrySingleCycle
import SampleInstructions

((cl, pc, rim, mctrl, rrf, aluctrl), cct) = makeCircuit tryControl

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleInstControlInstructions

--	x15	?		(1111111111, ?)
--	x2	x1		(9876543210, 1234567890)
--	x1	x2		(1234567890, 9876543210)
--	x10	x15		(9999999999, 1111111111)
--	x30	x31		(7777777777, 7777777777)

cct2 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct1 $ zip
	[1, 2, 10, 15, 30, 31] [
		1234567890,	-- 1
		9876543210,	-- 2
		9999999999,	-- 10
		1111111111,	-- 15
		7777777777,	-- 30
		7777777777 ]	-- 31

cct3 = resetMainController mctrl cct2

cct4 = clockOn cl cct3

cct5 = resetProgramCounter pc cct4

cct6 = stopProgramCounter pc 255 cct5
