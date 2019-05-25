{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FastAdderBinary where

import Control.Monad
import Data.Word

import Circuit
import Tools

type IWirePair = (IWire, IWire)
type OWirePair = (OWire, OWire)
type BitPair = (Bit, Bit)

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

type Carry2Wires = (IWire, IWirePair, IWirePair, OWire)

carry2 :: CircuitBuilder Carry2Wires
carry2 = do
	(a0, b0, g0out, p0out) <- generateGp
	(a1, b1, g1out, p1out) <- generateGp
	((g0', p0'), (g1', p1'), (rgout, rpout)) <- raiseGp
	(ci0, (rg', rp'), co2) <- generateCarry
	zipWithM_ connectWire
		[g0out, p0out, g1out, p1out, rgout, rpout]
		[g0', p0', g1', p1', rg', rp']
	return (ci0, (a0, a1), (b0, b1), co2)

setBitsCarry2 :: Carry2Wires -> Bit -> BitPair -> BitPair -> Circuit -> Circuit
setBitsCarry2 (ci0, (a0, a1), (b0, b1), _) bci0 (ba0, ba1) (bb0, bb1) =
	setBit ci0 bci0
		. foldr (.) id (
			zipWith setBit [a0, a1, b0, b1] [ba0, ba1, bb0, bb1] )

peekBitsCarry2 :: Carry2Wires -> Circuit -> Bit
peekBitsCarry2 (_, _, _, co2) = peekOWire co2

type Carry1_2Wires = (IWire, IWirePair, IWirePair, OWirePair)

carry1_2 :: CircuitBuilder Carry1_2Wires
carry1_2 = do
	(ci0in, ci0out) <- idGate
	(a0, b0, g0out, p0out) <- generateGp
	(a1, b1, g1out, p1out) <- generateGp
	((g0', p0'), (g1', p1'), (rgout, rpout)) <- raiseGp
	(ci0', (g0'', p0''), co1) <- generateCarry
	(ci0'', (rg', rp'), co2) <- generateCarry
	zipWithM_ connectWire
		[g0out, p0out, g1out, p1out,
			ci0out, g0out, p0out, ci0out, rgout, rpout]
		[g0', p0', g1', p1', ci0', g0'', p0'', ci0'', rg', rp']
	return (ci0in, (a0, a1), (b0, b1), (co1, co2))

setBitsCarry1_2 ::
	Carry1_2Wires -> Bit -> BitPair -> BitPair -> Circuit -> Circuit
setBitsCarry1_2 (ci0, (a0, a1), (b0, b1), _) bci0 (ba0, ba1) (bb0, bb1) =
	setBit ci0 bci0
		. foldr (.) id (
			zipWith setBit [a0, a1, b0, b1] [ba0, ba1, bb0, bb1] )

peekBitsCarry1_2 :: Carry1_2Wires -> Circuit -> BitPair
peekBitsCarry1_2 (_, _, _, (co1, co2)) = (,) <$> peekOWire co1 <*> peekOWire co2

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

setBitsCarry1_2n ::
	Carry1_2nWires -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setBitsCarry1_2n (ci0, as, bs, _, _, _) bci0 a b = setBit ci0 bci0
	. foldr (.) id (zipWith setBit as $ wordToBits 64 a)
	. foldr (.) id (zipWith setBit bs $ wordToBits 64 b)

peekBitsCarry1_2n :: Carry1_2nWires -> Circuit -> [Bit]
peekBitsCarry1_2n (_, _, _, co, _, _) cct = (`peekOWire` cct) <$> co
