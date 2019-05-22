{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FastAdder where

import Control.Arrow
import Control.Monad

import Circuit

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
	(IWire, (IWirePair, IWirePair, IWirePair, IWirePair), OWire)

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
	return (c0, ((a0, b0), (a1, b1), (a2, b2), (a3, b3)), o5o)

setBitsFastCarry4 :: FastCarry4Wires ->
	Bit -> BitPair -> BitPair -> BitPair -> BitPair -> Circuit -> Circuit
setBitsFastCarry4 (c0, ((a0, b0), (a1, b1), (a2, b2), (a3, b3)), _)
		bc0 (ba0, bb0) (ba1, bb1) (ba2, bb2) (ba3, bb3) = foldr (.) id
	$ zipWith setBit
		[c0, a0, b0, a1, b1, a2, b2, a3, b3]
		[bc0, ba0, bb0, ba1, bb1, ba2, bb2, ba3, bb3]

peekBitsFastCarry4 :: FastCarry4Wires -> Circuit -> Bit
peekBitsFastCarry4 (_, _, o) = peekOWire o

--------------------------------------------------------------------------------

run :: Int -> Circuit -> Circuit
run n = (!! n) . iterate step

setBits31 :: (IWire, IWire, IWire, OWire) ->
	Bit -> Bit -> Bit -> Circuit -> Circuit
setBits31 (i1, i2, i3, _) b1 b2 b3 =
	foldr (.) id $ zipWith setBit [i1, i2, i3] [b1, b2, b3]

peekBits31 :: (IWire, IWire, IWire, OWire) -> Circuit -> Bit
peekBits31 (_, _, _, o) = peekOWire o

orGate5, andGate5 :: CircuitBuilder ((IWire, IWire, IWire, IWire, IWire), OWire)
orGate5 = first listToTuple5 <$> multiOrGate 5
andGate5 = first listToTuple5 <$> multiAndGate 5

orGate4, andGate4 :: CircuitBuilder ((IWire, IWire, IWire, IWire), OWire)
orGate4 = first listToTuple4 <$> multiOrGate 4
andGate4 = first listToTuple4 <$> multiAndGate 4

orGate3, andGate3 :: CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate3 = first listToTuple3 <$> multiOrGate 3
andGate3 = first listToTuple3 <$> multiAndGate 3

multiOrGate, multiAndGate :: Int -> CircuitBuilder ([IWire], OWire)
multiOrGate = multiple orGate
multiAndGate = multiple andGate

multiple :: CircuitBuilder (IWire, IWire, OWire) ->
	Int -> CircuitBuilder ([IWire], OWire)
multiple _ n | n < 1 = error "Oops!"
multiple _ 1 = first (: []) <$> idGate
multiple g 2 = (\(i1, i2, o) -> ([i1, i2], o)) <$> g
multiple g n = do
	(is1, o1) <- multiple g (n `div` 2)
	(is2, o2) <- multiple g (n - n `div` 2)
	(i1, i2, o) <- g
	connectWire o1 i1
	connectWire o2 i2
	return (is1 ++ is2, o)

--------------------------------------------------------------------------------

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x, y] = (x, y)
listToTuple2 _ = error "Oops!"

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "Oops!"

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [x, y, z, w] = (x, y, z, w)
listToTuple4 _ = error "Oops!"

listToTuple5 :: [a] -> (a, a, a, a, a)
listToTuple5 [x, y, z, w, v] = (x, y, z, w, v)
listToTuple5 _ = error "Oops!"
