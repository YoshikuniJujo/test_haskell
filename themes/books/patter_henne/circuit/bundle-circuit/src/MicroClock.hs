-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module MicroClock () where

import Circuit
import Element
import Clock

data Register = Register {
	rgSwitch :: IWire, rgManualClock :: IWire, rgManualInput :: IWire,
	rgClock :: IWire, rgInput :: IWire, rgOutput :: OWire }

register :: CircuitBuilder Register
register = do
	(swin, swout) <- idGate0
	(sw, cc, mc, oc) <- mux2
	(sw', ci, mi, oi) <- mux2
	(c, d, q, _q_) <- dflipflop
	connectWire0 swout sw
	connectWire0 swout sw'
	connectWire0 oc c
	connectWire64 oi d
	return $ Register swin mc mi cc ci q

resetRegister :: Register -> Circuit -> Circuit
resetRegister rg cct = let
	cct1 = (!! 10) . iterate step $ setBits (rgSwitch rg) (Bits 1) cct
	cct2 = (!! 20) . iterate step
		. setBits (rgManualInput rg) (Bits 0)
		$ setBits (rgManualClock rg) (Bits 1) cct1
	cct3 = (!! 20) . iterate step $ setBits (rgManualClock rg) (Bits 0) cct2
	cct4 = (!! 10) . iterate step $ setBits (rgSwitch rg) (Bits 0) cct3 in
	cct4

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
	connectWire64 (rgOutput rg) ci
	connectWire64 co (rgInput rg)
	return (rg, co)

tryCycling :: CircuitBuilder (Clock, Register)
tryCycling = do
	cl <- clock 10
	(rg, _) <- cycling
	connectWire0 (clockSignal cl) (rgClock rg)
	return (cl, rg)

unit1 :: CircuitBuilder (Register, IWire, IWire, IWire, OWire)
unit1 = do
	(mcin, mcout) <- idGate0
	(sl1, ec, mc, co) <- mux2
	connectWire0 mcout mc
	(sl2, one', end', cd) <- mux2
	one <- constGate0 $ Bits 1
	(ein, eout) <- nand01
	connectWire0 one one'
	connectWire0 eout end'
	rg <- register
	connectWire0 co (rgClock rg)
	connectWire0 cd (rgInput rg)
	connectWire0 (rgOutput rg) sl1
	connectWire0 (rgOutput rg) sl2
	(a, b, o) <- andGate0
	connectWire0 (rgOutput rg) a
	connectWire0 mcout b
	return (rg, ec, mcin, ein, o)

useMicroClock :: CircuitBuilder (Register, Register, IWire, IWire)
useMicroClock = do
	(st, o) <- cycling
	(r, ec, mc, e, cs) <- unit1
	connectWire0 cs (rgClock st)
	connectWire64 o e
	return (r, st, ec, mc)

tryMicroClock :: CircuitBuilder (Clock, Clock, Register, Register)
tryMicroClock = do
	ec <- clock 255
	mc <- clock 20
	(r, st, ecin, mcin) <- useMicroClock
	connectWire0 (clockSignal ec) ecin
	connectWire0 (clockSignal mc) mcin
	return (ec, mc, r, st)
