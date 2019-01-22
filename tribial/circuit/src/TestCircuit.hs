{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TestCircuit where

import Control.Arrow
import Control.Monad
import Data.Bool
import Data.Bits ((.|.), testBit, shiftL, shiftR)
import Data.Word

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

--------------------------------------------------------------------------------

multiplexer :: Int -> CircuitBuilder (IWire, [IWire], [IWire], [OWire])
multiplexer n = do
	(si, so) <- idGate
	(as, bs, cs) <- unzip3 <$> replicateM n (mux so)
	return (si, as, bs, cs)

mux :: OWire -> CircuitBuilder (IWire, IWire, OWire)
mux so = do
	(a, s1, a1o) <- andGate
	(b, s2, a2o) <- andGate
	(oi1, oi2, c) <- orGate
	(ni, no) <- notGate
	connectWire so ni
	connectWire no s1
	connectWire so s2
	connectWire a1o oi1
	connectWire a2o oi2
	return (a, b, c)

inputMultiplexer :: (IWire, [IWire], [IWire], [OWire]) ->
	(Bit, Word64, Word64) -> Circuit -> Circuit
inputMultiplexer (ws, was, wbs, _) (bs, bas, bbs) cct = setBit ws bs
	. (flip . foldr) (uncurry setBit) (zip was $ wordToBits 64 bas)
	$ (flip . foldr) (uncurry setBit) (zip wbs $ wordToBits 64 bbs) cct

peekMultiplexer :: (IWire, [IWire], [IWire], [OWire]) -> Circuit -> [Bit]
peekMultiplexer (_, _, _, os) cct = map (`peekOWire` cct) os

wordToBits :: Int -> Word64 -> [Bit]
wordToBits n _ | n < 1 = []
wordToBits n w = bool O I (w `testBit` 0) : wordToBits (n - 1) (w `shiftR` 1)

bitsToWord :: [Bit] -> Word64
bitsToWord [] = 0
bitsToWord (b : bs) = (case b of O -> 0; I -> 1) .|. bitsToWord bs `shiftL` 1

xorGate :: CircuitBuilder (IWire, IWire, OWire)
xorGate = do
	(ai, ao) <- idGate
	(bi, bo) <- idGate
	(ai1, ai2, ado) <- andGate
	(oi1, oi2, oro) <- orGate
	(ni, no) <- notGate
	(aai1, aai2, aado) <- andGate
	connectWire ao ai1
	connectWire bo ai2
	connectWire ado ni
	connectWire ao oi1
	connectWire bo oi2
	connectWire no aai1
	connectWire oro aai2
	return (ai, bi, aado)

halfAdder :: CircuitBuilder (IWire, IWire, OWire, OWire)
halfAdder = do
	(ai, ao) <- idGate
	(bi, bo) <- idGate
	(si1, si2, s) <- xorGate
	(ci1, ci2, c) <- andGate
	connectWire ao si1
	connectWire bo si2
	connectWire ao ci1
	connectWire bo ci2
	return (ai, bi, s, c)

inputHalfAdder ::
	(IWire, IWire, OWire, OWire) -> (Bit, Bit) -> Circuit -> Circuit
inputHalfAdder (a, b, _, _) (ab, bb) = setBit a ab . setBit b bb

peekHalfAdder ::
	(IWire, IWire, OWire, OWire) -> Circuit -> (Bit, Bit)
peekHalfAdder (_, _, s, c) = (,) <$> peekOWire s <*> peekOWire c

all4AndGate :: OWire -> OWire -> CircuitBuilder [OWire]
all4AndGate o1 o2 = do
	(is, os) <- unzip . map (\(a, b, c) -> ([a, b], c))
		<$> replicateM 4 andGate
	sequence_ . concat $ zipWith
		(flip (zipWith3 id) [o1, o2]) (patterns invert obvert 2) is
	return os

testAll4AndGate :: CircuitBuilder (IWire, IWire, [OWire])
testAll4AndGate = do
	(i1, o1) <- idGate
	(i2, o2) <- idGate
	os <- all4AndGate o1 o2
	return (i1, i2, os)

setTestAll4AndGate ::
	(IWire, IWire, [OWire]) -> (Bit, Bit) -> Circuit -> Circuit
setTestAll4AndGate (i1, i2, _) (b1, b2) = setBit i1 b1 . setBit i2 b2

peekTestAll4AndGate :: (IWire, IWire, [OWire]) -> Circuit -> [Bit]
peekTestAll4AndGate (_, _, os) cct = map (`peekOWire` cct) os

m4to1 :: OWire -> OWire -> CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
m4to1 s0 s1 = do
	ss <- all4AndGate s0 s1
	(as, bs, os) <- unzip3 <$> replicateM 4 andGate
	zipWithM_ connectWire ss as
	(ois, oo) <- multiOrGate 4
	zipWithM_ connectWire os ois
	return $ (\[a, b, c, d] -> (a, b, c, d, oo)) bs

testM4to1 :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, IWire, OWire)
testM4to1 = do
	(si1, so1) <- idGate
	(si2, so2) <- idGate
	(a, b, c, d, o) <- m4to1 so1 so2
	return (a, b, c, d, si1, si2, o)

setTestM4to1 :: (IWire, IWire, IWire, IWire, IWire, IWire, OWire) ->
	(Bit, Bit, Bit, Bit, Bit, Bit) -> Circuit -> Circuit
setTestM4to1 (a, b, c, d, s1, s2, _) (ba, bb, bc, bd, bs1, bs2) =
	flip (foldr $ uncurry setBit)
		[(a, ba), (b, bb), (c, bc), (d, bd), (s1, bs1), (s2, bs2)]

peekTestM4to1 ::
	(IWire, IWire, IWire, IWire, IWire, IWire, OWire) -> Circuit -> Bit
peekTestM4to1 (_, _, _, _, _, _, o) = peekOWire o

unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 [] = ([], [], [], [], [])
unzip5 ((a, b, c, d, e) : fs) =
	(a : as, b : bs, c : cs, d : ds, e : es)
	where (as, bs, cs, ds, es) = unzip5 fs

mult4to1 :: CircuitBuilder ([IWire], [IWire], [IWire], [IWire], IWire, IWire, [OWire])
mult4to1 = do
	(s1, so1) <- idGate
	(s2, so2) <- idGate
	(as, bs, cs, ds, os) <- unzip5 <$> replicateM 64 (m4to1 so1 so2)
	return (as, bs, cs, ds, s1, s2, os)

wordTo2Bit :: Word8 -> (Bit, Bit)
wordTo2Bit w = (bool O I $ w `testBit` 1, bool O I $ w `testBit` 0)

setMult4to1 :: ([IWire], [IWire], [IWire], [IWire], IWire, IWire, [OWire]) ->
	(Word64, Word64, Word64, Word64, Word8) -> Circuit -> Circuit
setMult4to1 (a, b, c, d, s0, s1, _) (wa, wb, wc, wd, ws) =
	(flip . foldr $ uncurry setBit)
			(zip (concat [a, b, c, d]) (concat [ba, bb, bc, bd]))
		. setBit s0 bs0 . setBit s1 bs1
	where
	[ba, bb, bc, bd] = map (wordToBits 64) [wa, wb, wc, wd]
	(bs0, bs1) = wordTo2Bit ws

peekMult4to1 :: ([IWire], [IWire], [IWire], [IWire], IWire, IWire, [OWire]) ->
	Circuit -> [Bit]
peekMult4to1 (_, _, _, _, _, _, o) = (<$> o) . flip peekOWire

andOr1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
andOr1 = do
	(ia, oa) <- idGate
	(ib, ob) <- idGate
	(ia1, ia2, oad) <- andGate
	(io1, io2, oo) <- orGate
	(m1, m2, s, om) <- mux2
	connectWire oa ia1
	connectWire ob ia2
	connectWire oa io1
	connectWire ob io2
	connectWire oad m1
	connectWire oo m2
	return (ia, ib, s, om)

setAndOr1 ::
	(IWire, IWire, IWire, OWire) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setAndOr1 (a, b, s, _) (ba, bb, bs) = setBit a ba . setBit b bb . setBit s bs

peekAndOr1 :: (IWire, IWire, IWire, OWire) -> Circuit -> Bit
peekAndOr1 (_, _, _, o) = peekOWire o
