{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryControl () where

import Data.Word

import Circuit
import Clock
import Memory
import SampleInstructions

import Control

tryControl :: CircuitBuilder (Clock, Register, IWire)
tryControl = do
	cl <- clock 15
	(r, inst, _, _) <- control
	connectWire0 (clockSignal cl) (registerClock r)
	return (cl, r, inst)

cctTryControl :: Word64 -> (OWire, Circuit)
cctTryControl si = let
	((cl, rg, inst), cct) = makeCircuit tryControl
	cct0 = setBits inst (Bits si) cct
	cct1 = resetRegister rg cct0
	cct2 = clockOn cl cct1 in
	(registerOutput rg, cct2)

tryMicroControl :: CircuitBuilder (Clock, Clock, Register, Register, IWire, OWire)
tryMicroControl = do
	ecl <- clock 240
	(mc, mcf, ecin, st, inst, out) <- microControl
	connectWire0 (clockSignal ecl) ecin
	return (ecl, mc, mcf, st, inst, out)

cctMicroControl :: Word64 -> (OWire, Circuit)
cctMicroControl si = let
	((ecl, mcl, mcf, st, inst, out), cct) = makeCircuit tryMicroControl
	cct0 = setBits inst (Bits si) cct
	cct1 = resetRegister mcf cct0
	cct2 = resetRegister st cct1
	cct3 = clockOn mcl cct2
	cct4 = clockOn ecl cct3 in
	(out, cct4)
