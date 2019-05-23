{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FastAdder where

import Control.Arrow
import Control.Monad

import Circuit
import Element
import Tools

rippleCarry1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
rippleCarry1 = do
	((cin, cout), (ain, aout), (bin, bout)) <-
		listToTuple3 <$> replicateM 3 idGate
	((a01, a02, ao0), (a11, a12, ao1), (a21, a22, ao2)) <-
		listToTuple3 <$> replicateM 3 andGate
	((o1, o2, o3), oo) <- first listToTuple3 <$> multiOrGate 3
	zipWithM_ connectWire
		[bout, cout, aout, cout, aout, bout]
		[a01, a02, a11, a12, a21, a22]
	zipWithM_ connectWire [ao0, ao1, ao2] [o1, o2, o3]
	return (cin, ain, bin, oo)

rippleCarry :: Int -> CircuitBuilder (IWire, [(IWire, IWire)], OWire)
rippleCarry n | n < 1 = error "Oops!"
rippleCarry 1 = do
	(c, a, b, o) <- rippleCarry1
	return (c, [(a, b)], o)
rippleCarry n = do
	(c1, a1, b1, o1) <- rippleCarry1
	(cn, abn, on) <- rippleCarry $ n - 1
	connectWire o1 cn
	return (c1, (a1, b1) : abn, on)

setBitsRippleCarry :: (IWire, [(IWire, IWire)], OWire) ->
	Bit -> [(Bit, Bit)] -> Circuit -> Circuit
setBitsRippleCarry (c, iabs, _) bc babs = setBit c bc
	.  foldr (.) id (zipWith
		(\(a, b) (ba, bb) -> setBit a ba . setBit b bb) iabs babs )

peekBitsRippleCarry :: (IWire, [(IWire, IWire)], OWire) -> Circuit -> Bit
peekBitsRippleCarry (_, _, o) = peekOWire o

--------------------------------------------------------------------------------

fastCarry1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
fastCarry1 = do
	(ain, bin, g, p) <- gp
	(a1, c, ao) <- andGate
	(o1, o2, oo) <- orGate
	zipWithM_ connectWire [g, p, ao] [o1, a1, o2]
	return (ain, bin, c, oo)

gp :: CircuitBuilder (IWire, IWire, OWire, OWire)
gp = do	((ain, aout), (bin, bout)) <- listToTuple2 <$> replicateM 2 idGate
	(g1, g2, g) <- andGate
	(p1, p2, p) <- orGate
	zipWithM_ connectWire [aout, bout, aout, bout] [g1, g2, p1, p2]
	return (ain, bin, g, p)

type IWirePair = (IWire, IWire)
type BitPair = (Bit, Bit)

type FastCarry2Wires = (IWire, (IWirePair, IWirePair), OWire)

fastCarry2 :: CircuitBuilder FastCarry2Wires
fastCarry2 = do
	(a0, b0, g0, p0) <- gp
	(a1, b1, g1, p1) <- gp
	(and1, and2, ando) <- andGate
	((and31, and32, c0), and3o) <- andGate3
	((or31, or32, or33), or3o) <- orGate3
	zipWithM_ connectWire
		[g1, p1, g0, ando, p1, p0, and3o]
		[or31, and1, and2, or32, and31, and32, or33]
	return (c0, ((a0, b0), (a1, b1)), or3o)

setBitsFastCarry2 ::
	FastCarry2Wires -> Bit -> BitPair -> BitPair -> Circuit -> Circuit
setBitsFastCarry2 (c0, ((a0, b0), (a1, b1)), _) bc0 (ba0, bb0) (ba1, bb1) =
	foldr (.) id
		$ zipWith setBit [c0, a0, b0, a1, b1] [bc0, ba0, bb0, ba1, bb1]

peekBitsFastCarry2 :: FastCarry2Wires -> Circuit -> Bit
peekBitsFastCarry2 (_, _, o) = peekOWire o

type FastCarry3Wires = (IWire, (IWirePair, IWirePair, IWirePair), OWire)

fastCarry3 :: CircuitBuilder FastCarry3Wires
fastCarry3 = do
	(a0, b0, g0, p0) <- gp
	(a1, b1, g1, p1) <- gp
	(a2, b2, g2, p2) <- gp
	(a21, a22, a2o) <- andGate
	((a31, a32, a33), a3o) <- andGate3
	((a41, a42, a43, c0), a4o) <- andGate4
	((o41, o42, o43, o44), o4o) <- orGate4
	zipWithM_ connectWire
		[g2, p2, g1, a2o, p2, p1, g0, a3o, p2, p1, p0, a4o]
		[o41, a21, a22, o42, a31, a32, a33, o43, a41, a42, a43, o44]
	return (c0, ((a0, b0), (a1, b1), (a2, b2)), o4o)

setBitsFastCarry3 :: FastCarry3Wires ->
	Bit -> BitPair -> BitPair -> BitPair -> Circuit -> Circuit
setBitsFastCarry3 (c0, ((a0, b0), (a1, b1), (a2, b2)), _)
		bc0 (ba0, bb0) (ba1, bb1) (ba2, bb2) = foldr (.) id
	$ zipWith setBit
		[c0, a0, b0, a1, b1, a2, b2] [bc0, ba0, bb0, ba1, bb1, ba2, bb2]

peekBitsFastCarry3 :: FastCarry3Wires -> Circuit -> Bit
peekBitsFastCarry3 (_, _, o) = peekOWire o

type FastCarry4Wires =
	(IWire, (IWirePair, IWirePair, IWirePair, IWirePair), OWire, OWire, OWire)

fastCarry4 :: CircuitBuilder FastCarry4Wires
fastCarry4 = do
	(a0, b0, g0, p0) <- gp
	(a1, b1, g1, p1) <- gp
	(a2, b2, g2, p2) <- gp
	(a3, b3, g3, p3) <- gp
	(a21, a22, a2o) <- andGate
	((a31, a32, a33), a3o) <- andGate3
	((a41, a42, a43, a44), a4o) <- andGate4
	((a51, a52, a53, a54, c0), a5o) <- andGate5
	((o51, o52, o53, o54, o55), o5o) <- orGate5
	zipWithM_ connectWire
		[g3, p3, g2, a2o, p3, p2, g1, a3o, p3, p2, p1, g0, a4o, p3, p2, p1, p0, a5o]
		[o51, a21, a22, o52, a31, a32, a33, o53, a41, a42, a43, a44, o54, a51, a52, a53, a54, o55]
	((p0in, g0in), (p1in, g1in), (p2in, g2in), (p3in, g3in), lp, lg) <- largePg
	zipWithM_ connectWire
		[p0, g0, p1, g1, p2, g2, p3, g3]
		[p0in, g0in, p1in, g1in, p2in, g2in, p3in, g3in]
	return (c0, ((a0, b0), (a1, b1), (a2, b2), (a3, b3)), o5o, lp, lg)

setBitsFastCarry4 :: FastCarry4Wires ->
	Bit -> BitPair -> BitPair -> BitPair -> BitPair -> Circuit -> Circuit
setBitsFastCarry4 (c0, ((a0, b0), (a1, b1), (a2, b2), (a3, b3)), _, _, _)
		bc0 (ba0, bb0) (ba1, bb1) (ba2, bb2) (ba3, bb3) = foldr (.) id
	$ zipWith setBit
		[c0, a0, b0, a1, b1, a2, b2, a3, b3]
		[bc0, ba0, bb0, ba1, bb1, ba2, bb2, ba3, bb3]

peekBitsFastCarry4 :: FastCarry4Wires -> Circuit -> (Bit, Bit, Bit)
peekBitsFastCarry4 (_, _, o, p, g) =
	(,,) <$> peekOWire o <*> peekOWire p <*> peekOWire g

type FastCarry1to4Wires = (
	IWire, (IWirePair, IWirePair, IWirePair, IWirePair),
	(OWire, OWire, OWire, OWire), OWire, OWire )

fastCarry1to4 :: CircuitBuilder FastCarry1to4Wires
fastCarry1to4 = do
	(c0in, c0out) <- idGate
	((a0in, a0out), (a1in, a1out), (a2in, a2out), (a3in, a3out)) <-
		listToTuple4 <$> replicateM 4 idGate
	((b0in, b0out), (b1in, b1out), (b2in, b2out), (b3in, b3out)) <-
		listToTuple4 <$> replicateM 4 idGate
	(a10, b10, c10, co1) <- fastCarry1
	(c20, ((a20, b20), (a21, b21)), co2) <- fastCarry2
	(c30, ((a30, b30), (a31, b31), (a32, b32)), co3) <- fastCarry3
	(c40, ((a40, b40), (a41, b41), (a42, b42), (a43, b43)), co4, lp, lg) <-
		fastCarry4
	mapM_ (connectWire c0out) [c10, c20, c30, c40]
	zipWithM_ connectWire
		[a0out, a0out, a0out, a0out,
			a1out, a1out, a1out, a2out, a2out, a3out]
		[a10, a20, a30, a40, a21, a31, a41, a32, a42, a43]
	zipWithM_ connectWire
		[b0out, b0out, b0out, b0out,
			b1out, b1out, b1out, b2out, b2out, b3out]
		[b10, b20, b30, b40, b21, b31, b41, b32, b42, b43]
	return (c0in, ((a0in, b0in), (a1in, b1in), (a2in, b2in), (a3in, b3in)),
		(co1, co2, co3, co4), lp, lg)

setBitsFastCarry1to4 :: FastCarry1to4Wires -> Bit ->
	BitPair -> BitPair -> BitPair -> BitPair -> Circuit -> Circuit
setBitsFastCarry1to4 (ci0, ((a0, b0), (a1, b1), (a2, b2), (a3, b3)), _, _, _)
	bci0 (ba0, bb0) (ba1, bb1) (ba2, bb2) (ba3, bb3) = setBit ci0 bci0
		. foldr (.) id (zipWith setBit
			[a0, b0, a1, b1, a2, b2, a3, b3]
			[ba0, bb0, ba1, bb1, ba2, bb2, ba3, bb3])

peekBitsFastCarry1to4 ::
	FastCarry1to4Wires -> Circuit -> ((Bit, Bit, Bit, Bit), Bit, Bit)
peekBitsFastCarry1to4 (_, _, (ci1, ci2, ci3, ci4), lp, lg) cct =
	((,,,) <$> peekOWire ci1 <*> peekOWire ci2
			<*> peekOWire ci3 <*> peekOWire ci4 $ cct,
		peekOWire lp cct, peekOWire lg cct)

--------------------------------------------------------------------------------

largePg :: CircuitBuilder
	(IWirePair, IWirePair, IWirePair, IWirePair, OWire, OWire)
largePg = do
	((p3in, p3out), (p2in, p2out), (p1in, p1out)) <-
		listToTuple3 <$> replicateM 3 idGate
	((p3', p2', p1', p0), lp) <- andGate4
	(a21, g2, a2o) <- andGate
	((a31, a32, g1), a3o) <- andGate3
	((a41, a42, a43, g0), a4o) <- andGate4
	((g3, o42, o43, o44), lg) <- orGate4
	zipWithM_ connectWire
		[p3out, p2out, p1out,
			p3out, a2o, p3out, p2out, a3o, p3out, p2out, p1out, a4o]
		[p3', p2', p1', a21, o42, a31, a32, o43, a41, a42, a43, o44]
	return ((p0, g0), (p1in, g1), (p2in, g2), (p3in, g3), lp, lg)

largePgToCarry1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
largePgToCarry1 = do
	(p0, c0, ao) <- andGate
	(g0, o1, c1) <- orGate
	connectWire ao o1
	return (c0, p0, g0, c1)

largePgToCarry2 ::
	CircuitBuilder (IWire, ((IWire, IWire), (IWire, IWire)), OWire)
largePgToCarry2 = do
	(p1in, p1out) <- idGate
	(a21, g0, a2o) <- andGate
	((a31, p0, c0), a3o) <- andGate3
	((g1, o32, o33), c2) <- orGate3
	zipWithM_ connectWire [p1out, a2o, p1out, a3o] [a21, o32, a31, o33]
	return (c0, ((p0, g0), (p1in, g1)), c2)

largePgToCarry3 :: CircuitBuilder
	(IWire, ((IWire, IWire), (IWire, IWire), (IWire, IWire)), OWire)
largePgToCarry3 = do
	((p2in, p2out), (p1in, p1out)) <- listToTuple2 <$> replicateM 2 idGate
	(a21, g1, a2o) <- andGate
	((a31, a32, g0), a3o) <- andGate3
	((a41, a42, p0, c0), a4o) <- andGate4
	((g2, o42, o43, o44), c3) <- orGate4
	zipWithM_ connectWire
		[p2out, a2o, p2out, p1out, a3o, p2out, p1out, a4o]
		[a21, o42, a31, a32, o43, a41, a42, o44]
	return (c0, ((p0, g0), (p1in, g1), (p2in, g2)), c3)

largePgToCarry4 :: CircuitBuilder (
	IWire, ((IWire, IWire), (IWire, IWire), (IWire, IWire), (IWire, IWire)),
	OWire, OWire, OWire )
largePgToCarry4 = do
	((p3in, p3out), (p2in, p2out), (p1in, p1out), (p0in, p0out)) <-
		listToTuple4 <$> replicateM 4 idGate
	((g3in, g3out), (g2in, g2out), (g1in, g1out), (g0in, g0out)) <-
		listToTuple4 <$> replicateM 4 idGate
	(a21, a22, a2o) <- andGate
	((a31, a32, a33), a3o) <- andGate3
	((a41, a42, a43, a44), a4o) <- andGate4
	((a51, a52, a53, a54, c0), a5o) <- andGate5
	((o51, o52, o53, o54, o55), c4) <- orGate5
	zipWithM_ connectWire
		[p3out, g2out, a2o, p3out, p2out, g1out, a3o,
			p3out, p2out, p1out, g0out, a4o, p3out, p2out, p1out, p0out, a5o, g3out]
		[a21, a22, o52, a31, a32, a33, o53,
			a41, a42, a43, a44, o54, a51, a52, a53, a54, o55, o51]
	((lp0, lg0), (lp1, lg1), (lp2, lg2), (lp3, lg3), lp, lg) <- largePg
	zipWithM_ connectWire
		[p0out, g0out, p1out, g1out, p2out, g2out, p3out, g3out]
		[lp0, lg0, lp1, lg1, lp2, lg2, lp3, lg3]
	return (c0, ((p0in, g0in), (p1in, g1in), (p2in, g2in), (p3in, g3in)), c4, lp, lg)

fastCarry16 :: CircuitBuilder (IWire, [IWirePair], [OWire], OWire, OWire)
fastCarry16 = do
	(c0in, c0out) <- idGate
	(ci0, ((a0, b0), (a1, b1), (a2, b2), (a3, b3)),
		(co1, co2, co3, _co4), p0, g0) <- fastCarry1to4
	(ci4, ((a4, b4), (a5, b5), (a6, b6), (a7, b7)),
		(co5, co6, co7, _co8), p1, g1) <- fastCarry1to4
	(ci8, ((a8, b8), (a9, b9), (a10, b10), (a11, b11)),
		(co9, co10, co11, _co12), p2, g2) <- fastCarry1to4
	(ci12, ((a12, b12), (a13, b13), (a14, b14), (a15, b15)),
		(co13, co14, co15, _co16), p3, g3) <- fastCarry1to4
	(ci10, p10, g10, co4) <- largePgToCarry1
	(ci20, ((p20, g20), (p21, g21)), co8) <- largePgToCarry2
	(ci30, ((p30, g30), (p31, g31), (p32, g32)), co12) <- largePgToCarry3
	(ci40, ((p40, g40), (p41, g41), (p42, g42), (p43, g43)), co16, lp, lg) <-
		largePgToCarry4
	mapM_ (connectWire c0out) [ci0, ci10, ci20, ci30, ci40]
	zipWithM_ connectWire
		[p0, g0, p0, g0, p1, g1, p0, g0, p1, g1, p2, g2,
			p0, g0, p1, g1, p2, g2, p3, g3]
		[p10, g10, p20, g20, p21, g21, p30, g30, p31, g31, p32, g32,
			p40, g40, p41, g41, p42, g42, p43, g43]
	zipWithM_ connectWire [co4, co8, co12] [ci4, ci8, ci12]
	return (c0in,
		[	(a0, b0), (a1, b1), (a2, b2), (a3, b3),
			(a4, b4), (a5, b5), (a6, b6), (a7, b7),
			(a8, b8), (a9, b9), (a10, b10), (a11, b11),
			(a12, b12), (a13, b13), (a14, b14), (a15, b15) ],
		[	co1, co2, co3, co4, co5, co6, co7, co8,
			co9, co10, co11, co12, co13, co14, co15, co16 ], lp, lg)

setBitsFastCarry16 :: (IWire, [IWirePair], [OWire], OWire, OWire) ->
	Bit -> [BitPair] -> Circuit -> Circuit
setBitsFastCarry16 (c0, ps, _, _, _) bc0 bps = setBit c0 bc0
	. foldr (.) id (
		zipWith (\(a, b) (ba, bb) -> setBit a ba . setBit b bb) ps bps )

peekBitsFastCarry16 :: (IWire, [IWirePair], [OWire], OWire, OWire) ->
	Circuit -> ([Bit], Bit, Bit)
peekBitsFastCarry16 (_, _, co, lp, lg) cct =
	((`peekOWire` cct) <$> co,  peekOWire lp cct, peekOWire lg cct)

--------------------------------------------------------------------------------

fastCarry64 :: CircuitBuilder (IWire, [IWirePair], [OWire])
fastCarry64 = do
	(c0in, c0out) <- idGate
	(ci0, abi0_15, co1_16, p0_15, g0_15) <- fastCarry16
	(ci16, abi16_31, co17_32, p16_31, g16_31) <- fastCarry16
	(ci32, abi32_47, co33_48, p32_47, g32_47) <- fastCarry16
	(ci48, abi48_63, co49_64, p48_63, g48_63) <- fastCarry16
	(ci10, p10, g10, co16) <- largePgToCarry1
	(ci20, ((p20, g20), (p21, g21)), co32) <- largePgToCarry2
	(ci30, ((p30, g30), (p31, g31), (p32, g32)), co48) <- largePgToCarry3
	(ci40, ((p40, g40), (p41, g41), (p42, g42), (p43, g43)), _co64, _lp, _lg) <-
		largePgToCarry4
	mapM_ (connectWire c0out) [ci0, ci10, ci20, ci30, ci40]
	zipWithM_ connectWire
		[p0_15, g0_15, p0_15, g0_15, p16_31, g16_31,
			p0_15, g0_15, p16_31, g16_31, p32_47, g32_47,
			p0_15, g0_15, p16_31, g16_31, p32_47, g32_47,
			p48_63, g48_63]
		[p10, g10, p20, g20, p21, g21, p30, g30, p31, g31, p32, g32,
			p40, g40, p41, g41, p42, g42, p43, g43]
	zipWithM_ connectWire [co16, co32, co48] [ci16, ci32, ci48]
	return (c0in, abi0_15 ++ abi16_31 ++ abi32_47 ++ abi48_63,
		co1_16 ++ co17_32 ++ co33_48 ++ co49_64)

setBitsFastCarry64 ::
	(IWire, [IWirePair], [OWire]) -> Bit -> [BitPair] -> Circuit -> Circuit
setBitsFastCarry64 (ci0, ab, _) bci0 bab = setBit ci0 bci0 . foldr (.) id (
	zipWith (\(a, b) (ba, bb) -> setBit a ba . setBit b bb) ab bab )

peekBitsFastCarry64 :: (IWire, [IWirePair], [OWire]) -> Circuit -> [Bit]
peekBitsFastCarry64 (_, _, co) cct = (`peekOWire` cct) <$> co
