{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MicroClock (
	Register, register, registerClock, registerInput, registerOutput,
		resetRegister,
	microClocked ) where

import Data.Word

import Circuit
import Element
import Clock

data Register = Register {
	rgSwitch :: IWire, rgManualClock :: IWire, rgManualInput :: IWire,
	rgClock :: IWire, rgInput :: IWire, rgOutput :: OWire }

registerClock, registerInput :: Register -> IWire
registerClock = rgClock
registerInput = rgInput

registerOutput :: Register -> OWire
registerOutput = rgOutput

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

unit1 :: CircuitBuilder (Register, IWire, IWire, IWire, OWire)
unit1 = do
	(mcin, mcout) <- idGate0
	(sl1, ec, mc, co) <- mux2
	connectWire0 mcout mc
	(sl2, one', end', cd) <- mux2
	one <- constGate0 $ Bits 1
	connectWire0 one one'
	rg <- register
	connectWire0 co (rgClock rg)
	connectWire0 cd (rgInput rg)
	connectWire0 (rgOutput rg) sl1
	connectWire0 (rgOutput rg) sl2
	(a, b, o) <- andGate0
	connectWire0 (rgOutput rg) a
	connectWire0 mcout b
	return (rg, ec, mcin, end', o)

microClocked :: Word8 -> IWire -> OWire -> CircuitBuilder (Clock, Register, IWire)
microClocked n cin chk = do
	mc <- clock n
	(r, ec, mcin, e, cs) <- unit1
	connectWire0 (clockSignal mc) mcin
	connectWire0 cs cin
	connectWire64 chk e
	return (mc, r, ec)
