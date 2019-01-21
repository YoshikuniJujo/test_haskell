{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TestCircuit where

import Control.Arrow

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
