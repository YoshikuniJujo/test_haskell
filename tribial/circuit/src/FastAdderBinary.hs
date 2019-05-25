{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FastAdderBinary (carry1_2n) where

import Control.Monad
import Data.Word

import Circuit
import Tools

type IWirePair = (IWire, IWire)
type OWirePair = (OWire, OWire)

generateGp :: CircuitBuilder (IWire, IWire, OWire, OWire)
generateGp = do
	((ain, aout), (bin, bout)) <- listToTuple2 <$> replicateM 2 idGate
	(a1, a2, g) <- andGate
	(o1, o2, p) <- orGate
	zipWithM_ connectWire [aout, bout, aout, bout] [a1, a2, o1, o2]
	return (ain, bin, g, p)

raiseGp :: CircuitBuilder (IWirePair, IWirePair, OWirePair)
raiseGp = do
	(p1in, p1out) <- idGate
	(p1', g0, ao) <- andGate
	(g1, o2, rg) <- orGate
	(p1'', p0, rp) <- andGate
	zipWithM_ connectWire [p1out, ao, p1out] [p1', o2, p1'']
	return ((g0, p0), (g1, p1in), (rg, rp))

generateCarry :: CircuitBuilder (IWire, IWirePair, OWire)
generateCarry = do
	(p, ci, ao) <- andGate
	(g, o2, co) <- orGate
	connectWire ao o2
	return (ci, (g, p), co)

type Carry1_2nWires = (IWire, [IWire], [IWire], [OWire], OWire, OWire)

carry1_2n :: Word8 -> CircuitBuilder Carry1_2nWires
carry1_2n 0 = do
	(a0, b0, g0out, p0out) <- generateGp
	(ci0, (g0', p0'), co1) <- generateCarry
	zipWithM_ connectWire [g0out, p0out] [g0', p0']
	return (ci0, [a0], [b0], [co1], g0out, p0out)
carry1_2n n = do
	(ci0in, ci0out) <- idGate
	(ci0', a0, b0, co0, g0out, p0out) <- carry1_2n $ n - 1
	(cin, an, bn, con, gnout, pnout) <- carry1_2n $ n - 1
	((g0', p0'), (gn', pn'), (rgout, rpout)) <- raiseGp
	(ci0'', (g0'', p0''), conout) <- generateCarry
	zipWithM_ connectWire
		[ci0out, g0out, p0out, gnout, pnout,
			ci0out, g0out, p0out, conout]
		[ci0', g0', p0', gn', pn', ci0'', g0'', p0'', cin]
	return (ci0in, a0 ++ an, b0 ++ bn, co0 ++ con, rgout, rpout)
