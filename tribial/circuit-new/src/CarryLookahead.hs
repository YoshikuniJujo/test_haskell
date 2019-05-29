{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CarryLookahead where

import Control.Monad
import Data.Word

import Circuit
import CircuitTools
import Tools
import RiscV.Tools

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
	(p1g, g0, p1g0) <- andGate
	(g1, p1g0', rg) <- orGate
	(p1p, p0, rp) <- andGate
	zipWithM_ connectWire [p1out, p1g0, p1out] [p1g, p1g0', p1p]
	return ((g0, p0), (g1, p1in), (rg, rp))

generateCarry :: CircuitBuilder (IWire, IWirePair, OWire)
generateCarry = do
	(p, ci, pci) <- andGate
	(g, pci', co) <- orGate
	connectWire pci pci'
	return (ci, (g, p), co)

type CarriesWires = (IWire, [IWire], [IWire], [OWire], OWire, OWire)

carries :: Word8 -> CircuitBuilder CarriesWires
carries 0 = error "carries: Oops!"
carries 1 = do
	(a0, b0, g0out, p0out) <- generateGp
	(ci0, (g0, p0), co1) <- generateCarry
	zipWithM_ connectWire [g0out, p0out] [g0, p0]
	return (ci0, [a0], [b0], [co1], g0out, p0out)
carries n = do
	(ci0in, ci0out) <- idGate
	(ci0, al, bl, col, glout, plout) <- carries $ n `div` 2
	(cih, ah, bh, coh, ghout, phout) <- carries $ n - n `div` 2
	((gl, pl), (gh, ph), (rgout, rpout)) <- raiseGp
	(ci0', (g0', p0'), conout) <- generateCarry
	zipWithM_ connectWire
		[ci0out, glout, plout, ghout, phout,
			ci0out, glout, plout, conout]
		[ci0, gl, pl, gh, ph, ci0', g0', p0', cih]
	return (ci0in, al ++ ah, bl ++ bh, col ++ coh, rgout, rpout)

setBitsCarries :: CarriesWires -> Bit -> Word64 -> Word64 -> DoCircuit
setBitsCarries (wci, was, wbs, _, _, _) bci a b = setBit wci bci
	. setBits was (numToBits 64 a) . setBits wbs (numToBits 64 b)

getBitsCarries :: CarriesWires -> Circuit -> [Bit]
getBitsCarries (_, _, _, cs, _, _) cct = (`peekOWire` cct) <$> cs

setAndRunCarries :: CarriesWires -> Bit -> Word64 -> Word64 -> Int -> DoCircuit
setAndRunCarries ws bci a b n = run n . setBitsCarries ws bci a b
