{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryMicroClock () where

import Data.Word

import Circuit
import Element
import Clock
import MicroClock

nand01 :: CircuitBuilder (IWire, OWire)
nand01 = do
	(iin, iout) <- idGate64
	(a, b, o) <- nandGate0
	connectWire (iout, 1, 0) (a, 1, 0)
	connectWire (iout, 1, 1) (b, 1, 0)
	return (iin, o)

cycle4 :: CircuitBuilder (IWire, OWire)
cycle4 = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	(ni, no) <- notGate0
	(a, b, o) <- xorGate0
	connectWire (iout, 1, 0) (ni, 1, 0)
	connectWire (no, 1, 0) (oin, 1, 0)
	connectWire (iout, 1, 0) (a, 1, 0)
	connectWire (iout, 1, 1) (b, 1, 0)
	connectWire (o, 1, 0) (oin, 1, 1)
	return (iin, oout)

cycling :: CircuitBuilder (Register, OWire)
cycling = do
	rg <- register
	(ci, co) <- cycle4
	connectWire64 (registerOutput rg) ci
	connectWire64 co (registerInput rg)
	(ein, eout) <- nand01
	connectWire64 co ein
	return (rg, eout)

tryCycling :: CircuitBuilder (Clock, Register)
tryCycling = do
	cl <- clock 10
	(rg, _) <- cycling
	connectWire0 (clockSignal cl) (registerClock rg)
	return (cl, rg)

useMicroClock :: CircuitBuilder (Clock, Register, Register, IWire)
useMicroClock = do
	(st, o) <- cycling
	(mc, r, ec) <- microClocked 20 (registerClock st) o
	return (mc, r, st, ec)

tryMicroClock :: CircuitBuilder (Clock, Clock, Register, Register)
tryMicroClock = do
	ec <- clock 255
	(mc, r, st, ecin) <- useMicroClock
	connectWire0 (clockSignal ec) ecin
	return (ec, mc, r, st)
