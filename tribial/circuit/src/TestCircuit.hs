{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TestCircuit where

import Control.Arrow
import Control.Monad

import Circuit

--------------------------------------------------------------------------------

mux2 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2 = do
	(a, s1, a1o) <- andGate
	(b, s2, a2o) <- andGate
	(xi1, xi2, c) <- orGate
	(ni, no) <- notGate
	(si, so) <- idGate
	connectWire so ni
	connectWire no s1
	connectWire so s2
	connectWire a1o xi1
	connectWire a2o xi2
	return (a, b, si, c)

type SBit = (String, Bit)

getMux2Wires :: (IWire, IWire, IWire, OWire) ->
	Circuit -> ((SBit, SBit, SBit), SBit)
getMux2Wires (a, b, s, c) =
	(\[x, y, z] -> (x, y, z)) . zip ["a", "b", "s"]
		. (\ctt -> map (`peekIWire` ctt) [a, b, s]) &&&
	(("c" ,) . peekOWire c)

setMux2Wires ::
	(IWire, IWire, IWire, a) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setMux2Wires (a, b, s, _) (ba, bb, bs) = setBit a ba . setBit b bb . setBit s bs

and3Gate :: CircuitBuilder (IWire, IWire, IWire, OWire)
and3Gate = do
	(i1, i2, o0) <- andGate
	(i0, i3, o) <- andGate
	connectWire o0 i0
	return (i1, i2, i3, o)

invert, obvert :: OWire -> IWire -> CircuitBuilder ()
invert o i = do
	(ni, no) <- notGate
	connectWire o ni
	connectWire no i
obvert = connectWire

decoder :: CircuitBuilder (
	IWire, IWire, IWire,
	OWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire )
decoder = do
	(i1, oi1) <- idGate
	(i2, oi2) <- idGate
	(i3, oi3) <- idGate
	(i01, i02, i03, o0) <- and3Gate
	(i11, i12, i13, o1) <- and3Gate
	(i21, i22, i23, o2) <- and3Gate
	(i31, i32, i33, o3) <- and3Gate
	(i41, i42, i43, o4) <- and3Gate
	(i51, i52, i53, o5) <- and3Gate
	(i61, i62, i63, o6) <- and3Gate
	(i71, i72, i73, o7) <- and3Gate
	sequence_ $ zipWith3 id
		[invert, invert, invert] [oi1, oi2, oi3] [i01, i02, i03]
	sequence_ $ zipWith3 id
		[invert, invert, obvert] [oi1, oi2, oi3] [i11, i12, i13]
	sequence_ $ zipWith3 id
		[invert, obvert, invert] [oi1, oi2, oi3] [i21, i22, i23]
	sequence_ $ zipWith3 id
		[invert, obvert, obvert] [oi1, oi2, oi3] [i31, i32, i33]
	sequence_ $ zipWith3 id
		[obvert, invert, invert] [oi1, oi2, oi3] [i41, i42, i43]
	sequence_ $ zipWith3 id
		[obvert, invert, obvert] [oi1, oi2, oi3] [i51, i52, i53]
	sequence_ $ zipWith3 id
		[obvert, obvert, invert] [oi1, oi2, oi3] [i61, i62, i63]
	sequence_ $ zipWith3 id
		[obvert, obvert, obvert] [oi1, oi2, oi3] [i71, i72, i73]
	return (i1, i2, i3, o0, o1, o2, o3, o4, o5, o6, o7)

inputDecoder :: (
		IWire, IWire, IWire,
		OWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire) ->
	(Bit, Bit, Bit) -> Circuit -> Circuit
inputDecoder (i1, i2, i3, _, _, _, _, _, _, _, _) (b1, b2, b3) cct =
	setBit i1 b1 $ setBit i2 b2 $ setBit i3 b3 cct

peekDecoder :: (
		IWire, IWire, IWire,
		OWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire) ->
	Circuit -> (
		Bit, Bit, Bit,
		Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
peekDecoder (i1, i2, i3, o0, o1, o2, o3, o4, o5, o6, o7) cct = (
	peekIWire i1 cct, peekIWire i2 cct, peekIWire i3 cct,
	peekOWire o0 cct, peekOWire o1 cct, peekOWire o2 cct,
	peekOWire o3 cct, peekOWire o4 cct, peekOWire o5 cct,
	peekOWire o6 cct, peekOWire o7 cct )

multiOrGate :: Int -> CircuitBuilder ([IWire], OWire)
multiOrGate n | n < 1 = error "Oops!"
multiOrGate 1 = first (: []) <$> idGate
multiOrGate 2 = (\(i1, i2, o) -> ([i1, i2], o)) <$> orGate
multiOrGate n = do
	(is1, o1) <- multiOrGate (n `div` 2)
	(is2, o2) <- multiOrGate (n - n `div` 2)
	(i1, i2, o) <- orGate
	connectWire o1 i1
	connectWire o2 i2
	return (is1 ++ is2, o)

patterns :: a -> a -> Int -> [[a]]
patterns _ _ n | n < 1 = [[]]
patterns a b n = [x : xs | x <- [a, b], xs <- patterns a b (n - 1)]

all8AndGate :: OWire -> OWire -> OWire -> CircuitBuilder [OWire]
all8AndGate o1 o2 o3 = do
	(is, os) <- unzip . map (\(a, b, c, d) -> ([a, b, c], d))
		<$> replicateM 8 and3Gate
	sequence_ . concat $ zipWith
		(flip (zipWith3 id) [o1, o2, o3]) (patterns invert obvert 3) is
	return os

test8AndGate :: CircuitBuilder (IWire, IWire, IWire, [OWire])
test8AndGate = do
	((i1, o1), (i2, o2), (i3, o3)) <- (,,) <$> idGate <*> idGate <*> idGate
	os <- all8AndGate o1 o2 o3
	return (i1, i2, i3, os)

pla :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire, OWire)
pla = do
	((a, oa), (b, ob), (c, oc)) <- (,,) <$> idGate <*> idGate <*> idGate
	dcs <- all8AndGate oa ob oc
	(ids, d) <- multiOrGate 7
	(ies, e) <- multiOrGate 3
	(if1, f) <- idGate
	zipWithM_ connectWire (tail dcs) ids
	zipWithM_ connectWire (map (dcs !!) [3, 5, 6]) ies
	connectWire (last dcs) if1
	return (a, b, c, d, e, f)

peekPla :: (IWire, IWire, IWire, OWire, OWire, OWire) -> Circuit ->
	(Bit, Bit, Bit, Bit, Bit, Bit)
peekPla (a, b, c, d, e, f) cct = (\[x, y, z, u, v, w] -> (x, y, z, u, v, w))
	$ map (`peekIWire` cct) [a, b, c] ++ map (`peekOWire` cct) [d, e, f]

setPla :: (IWire, IWire, IWire, OWire, OWire, OWire) ->
	(Bit, Bit, Bit) -> Circuit -> Circuit
setPla (a, b, c, _, _, _) (ba, bb, bc) cct =
	setBit a ba . setBit b bb $ setBit c bc cct
