{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CarryLookahead where

import Control.Monad
import Data.Word

import Circuit

generateGp :: CircuitBuilder (IWire, IWire, OWire, OWire)
generateGp = do
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(aa, ab, g) <- andGate64
	(oa, ob, p) <- orGate64
	zipWithM_ connectWire64 [aout, bout, aout, bout] [aa, ab, oa, ob]
	return (ain, bin, g, p)

raiseGp :: OWire -> OWire -> IWire -> IWire -> Word8 -> CircuitBuilder ()
raiseGp gs ps rg rp p = do
	(p1g, g0, p1g0) <- andGate0
	(g1, p1g0', rgout) <- orGate0
	(p1p, p0, rpout) <- andGate0
	connectWire (ps, 1, p) (p0, 1, 0)
	connectWire (ps, 1, p + 1) (p1g, 1, 0)
	connectWire (ps, 1, p + 1) (p1p, 1, 0)
	connectWire (gs, 1, p) (g0, 1, 0)
	connectWire (gs, 1, p + 1) (g1, 1, 0)
	connectWire0 p1g0 p1g0'
	connectWire (rgout, 1, 0) (rg, 1, p `div` 2)
	connectWire (rpout, 1, 0) (rp, 1, p `div` 2)

testRaise :: CircuitBuilder (IWire, IWire, OWire, OWire)
testRaise = do
	(gin, gout) <- idGate64
	(pin, pout) <- idGate64
	(rgin, rgout) <- idGate64
	(rpin, rpout) <- idGate64
	raiseGp gout pout rgin rpin 0
	return (gin, pin, rgout, rpout)
