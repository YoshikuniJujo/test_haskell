{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MicroClock (
	Register, register, registerClock, registerInput, registerOutput,
		resetRegister,
	microClocked ) where

import Data.Word

import Circuit
import Element
import Memory
import Clock

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
