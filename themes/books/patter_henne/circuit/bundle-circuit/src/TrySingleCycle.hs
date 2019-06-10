{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySingleCycle where

import Circuit
import Clock
import Memory

tryProgramCounter :: CircuitBuilder (Clock, ProgramCounter)
tryProgramCounter = do
	cl <- clock 20
	pc <- programCounter
	pcClocked cl pc
	return (cl, pc)
