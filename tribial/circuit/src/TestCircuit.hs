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

decoder8 :: CircuitBuilder (
	IWire, IWire, IWire,
	OWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire )
decoder8 = do
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

decoder :: Int -> CircuitBuilder ([IWire], [OWire])
decoder n = do
	(is, ois) <- unzip <$> m `replicateM` idGate
	(ias, oas) <- unzip <$> n `replicateM` multiAndGate m
	zipWithM_ ((sequence_ .) . flip (zipWith3 id) ois) (binary (invert, obvert) m) ias
	return (is, oas)
	where m = log2 n

binary :: (a, a) -> Int -> [[a]]
binary _ n | n < 1 = [[]]
binary (o, i) n = binary (o, i) (n - 1) >>= (<$> [(o :), (i :)]) . flip ($)

log2 :: Integral n => n -> n
log2 i = l2 0
	where l2 j
		| j < 0 = error "bad"
		| 2 ^ j >= i = j
		| otherwise = l2 $ j + 1

setDecoder :: ([IWire], [OWire]) -> [Bit] -> Circuit -> Circuit
setDecoder (is, _) = foldr (.) id . zipWith setBit is

peekDecoder :: ([IWire], [OWire]) -> Circuit -> [Bit]
peekDecoder (_, os) cct = (`peekOWire` cct) <$> os

inputDecoder :: (
		IWire, IWire, IWire,
		OWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire) ->
	(Bit, Bit, Bit) -> Circuit -> Circuit
inputDecoder (i1, i2, i3, _, _, _, _, _, _, _, _) (b1, b2, b3) cct =
	setBit i1 b1 $ setBit i2 b2 $ setBit i3 b3 cct

peekDecoder8 :: (
		IWire, IWire, IWire,
		OWire, OWire, OWire, OWire, OWire, OWire, OWire, OWire) ->
	Circuit -> (
		Bit, Bit, Bit,
		Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
peekDecoder8 (i1, i2, i3, o0, o1, o2, o3, o4, o5, o6, o7) cct = (
	peekIWire i1 cct, peekIWire i2 cct, peekIWire i3 cct,
	peekOWire o0 cct, peekOWire o1 cct, peekOWire o2 cct,
	peekOWire o3 cct, peekOWire o4 cct, peekOWire o5 cct,
	peekOWire o6 cct, peekOWire o7 cct )

multiAndGate :: Int -> CircuitBuilder ([IWire], OWire)
multiAndGate n | n < 1 = error "Oops!"
multiAndGate 1 = first (: []) <$> idGate
multiAndGate 2 = (\(i1, i2, o) -> ([i1, i2], o)) <$> andGate
multiAndGate n = do
	(is1, o1) <- multiAndGate (n `div` 2)
	(is2, o2) <- multiAndGate (n - n `div` 2)
	(i1, i2, o) <- andGate
	connectWire o1 i1
	connectWire o2 i2
	return (is1 ++ is2, o)

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

multiplexer2 :: Int -> CircuitBuilder (IWire, [IWire], [IWire], [OWire])
multiplexer2 n = do
	(si, so) <- idGate
	(as, bs, cs) <- unzip3 <$> replicateM n (mx so)
	return (si, as, bs, cs)
	where mx so = do
		(a, b, si, c) <- mux21
		connectWire so si
		return (a, b, c)

mux21 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux21 = do
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

inputMultiplexer2 :: (IWire, [IWire], [IWire], [OWire]) ->
	(Bit, Word64, Word64) -> Circuit -> Circuit
inputMultiplexer2 (ws, was, wbs, _) (bs, bas, bbs) cct = setBit ws bs
	. (flip . foldr) (uncurry setBit) (zip was $ wordToBits 64 bas)
	$ (flip . foldr) (uncurry setBit) (zip wbs $ wordToBits 64 bbs) cct

peekMultiplexer2 :: (IWire, [IWire], [IWire], [OWire]) -> Circuit -> [Bit]
peekMultiplexer2 (_, _, _, os) cct = map (`peekOWire` cct) os

wordToBits :: Int -> Word64 -> [Bit]
wordToBits n _ | n < 1 = []
wordToBits n w = bool O I (w `testBit` 0) : wordToBits (n - 1) (w `shiftR` 1)

bitsToWord :: [Bit] -> Word64
bitsToWord [] = 0
bitsToWord (b : bs) = (case b of O -> 0; I -> 1) .|. bitsToWord bs `shiftL` 1

mux_1 :: Int -> CircuitBuilder ([IWire], [IWire], OWire)
mux_1 n = do
	(dis, dos) <- decoder n
	(ai1s, ai2s, aos) <- unzip3 <$> n `replicateM` andGate
	(ois, o) <- multiOrGate n
	zipWithM_ connectWire dos ai1s
	zipWithM_ connectWire aos ois
	return (dis, ai2s, o)

inputMux_1 :: ([IWire], [IWire], OWire) -> Word64 -> [Bit] -> Circuit -> Circuit
inputMux_1 (si, is, _) s bs = foldr (.) id (zipWith setBit si $ wordToBits 64 s)
	. foldr (.) id (zipWith setBit is bs)

peekMux_1 :: ([IWire], [IWire], OWire) -> Circuit -> Bit
peekMux_1 (_, _, o) = peekOWire o

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

{-
unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 ((a, b, c, d) : es) = (a : as, b : bs, c : cs, d : ds)
	where (as, bs, cs, ds) = unzip4 es
	-}

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
	(m1, m2, s, om) <- mux21
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

adder1Carry :: CircuitBuilder (IWire, IWire, IWire, OWire)
adder1Carry = do
	(ici, oci) <- idGate
	(ia, oa) <- idGate
	(ib, ob) <- idGate
	(ici1, ia1, o1) <- andGate
	(ici2, ib2, o2) <- andGate
	(ia3, ib3, o3) <- andGate
	(is, oco) <- multiOrGate 3
	case is of
		[i1, i2, i3] -> do
			connectWire oci ici1
			connectWire oci ici2
			connectWire oa ia1
			connectWire oa ia3
			connectWire ob ib2
			connectWire ob ib3
			connectWire o1 i1
			connectWire o2 i2
			connectWire o3 i3
			return (ici, ia, ib, oco)
		_ -> error "bad"

setAdder1Carry :: (IWire, IWire, IWire, OWire) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setAdder1Carry (c, a, b, _) (bc, ba, bb) = setBit c bc . setBit a ba . setBit b bb

peekAdder1Carry :: (IWire, IWire, IWire, OWire) -> Circuit -> Bit
peekAdder1Carry (_, _, _, co) = peekOWire co

adder1Sum :: CircuitBuilder (IWire, IWire, IWire, OWire)
adder1Sum = do
	(ici, oci) <- idGate
	(ia, oa) <- idGate
	(ib, ob) <- idGate
	(nici, noci) <- notGate
	(nia, noa) <- notGate
	(nib, nob) <- notGate
	(ici1, ia1, ib1, o1) <- and3Gate
	(ici2, ia2, ib2, o2) <- and3Gate
	(ici3, ia3, ib3, o3) <- and3Gate
	(ici4, ia4, ib4, o4) <- and3Gate
	(is, os) <- multiOrGate 4
	case is of
		[i1, i2, i3, i4] -> do
			connectWire oci nici
			connectWire oa nia
			connectWire ob nib
			connectWire oci ici1
			connectWire noa ia1
			connectWire nob ib1
			connectWire noci ici2
			connectWire oa ia2
			connectWire nob ib2
			connectWire noci ici3
			connectWire noa ia3
			connectWire ob ib3
			connectWire oci ici4
			connectWire oa ia4
			connectWire ob ib4
			connectWire o1 i1
			connectWire o2 i2
			connectWire o3 i3
			connectWire o4 i4
		_ -> error "bad"
	return (ici, ia, ib, os)

adder1 :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
adder1 = do
	(ici, oci) <- idGate
	(ia, oa) <- idGate
	(ib, ob) <- idGate
	(sici, sia, sib, s) <- adder1Sum
	(cici, cia, cib, c) <- adder1Carry
	connectWire oci sici
	connectWire oa sia
	connectWire ob sib
	connectWire oci cici
	connectWire oa cia
	connectWire ob cib
	return (ici, ia, ib, s, c)

setAdder1 :: (IWire, IWire, IWire, OWire, OWire) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setAdder1 (c, a, b, _, _) (bc, ba, bb) = setBit c bc . setBit a ba . setBit b bb

peekAdder1 :: (IWire, IWire, IWire, OWire, OWire) -> Circuit -> (Bit, Bit)
peekAdder1 (_, _, _, s, co) cct = (peekOWire s cct, peekOWire co cct)

alu1 :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, OWire, OWire)
alu1 = do
	(ain, a) <- idGate
	(bin, b) <- idGate
	(ai1, ai2, ao) <- andGate
	(oi1, oi2, oo) <- orGate
	(ci, adi1, adi2, s, co) <- adder1
	(op, mis, mo) <- mux_1 3
	case (op, mis) of
		([o0, o1], [mi0, mi1, mi2]) -> do
			connectWire a ai1
			connectWire b ai2
			connectWire a oi1
			connectWire b oi2
			connectWire a adi1
			connectWire b adi2
			connectWire ao mi0
			connectWire oo mi1
			connectWire s mi2
			return ((o0, o1), ci, ain, bin, mo, co)
		_ -> error "Oops!"

setAlu1 :: ((IWire, IWire), IWire, IWire, IWire, OWire, OWire) -> Word64 -> Bit -> Bit -> Bit -> Circuit -> Circuit
setAlu1 ((o0, o1), ci, a, b, _, _) op bci ba bb = foldr (.) id (zipWith setBit [o0, o1] $ wordToBits 64 op)
	. foldr (.) id (zipWith setBit [ci, a, b] [bci, ba, bb])

peekAlu1 :: ((IWire, IWire), IWire, IWire, IWire, OWire, OWire) -> Circuit -> (Bit, Bit)
peekAlu1 (_, _, _, _, r, co) cct = (peekOWire r cct, peekOWire co cct)

alu :: Int -> CircuitBuilder ((IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire)
alu n | n < 1 = error "Oops!"
alu 1 = do
	(op, ci, a, b, r, co) <- alu1
	return (op, ci, [a], [b], [r], co)
alu n = do
	(opin0, opout0) <- idGate
	(opin1, opout1) <- idGate
	((op10, op11), ci1, a1, b1, r1, co) <- alu1
	((opn0, opn1), cin, an, bn, rn, con) <- alu $ n - 1
	connectWire opout0 op10
	connectWire opout0 opn0
	connectWire opout1 op11
	connectWire opout1 opn1
	connectWire co cin
	return ((opin0, opin1), ci1, a1 : an, b1 : bn, r1 : rn, con)

setAlu :: ((IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) ->
	Word64 -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setAlu ((op0, op1), ci, a, b, _, _) op bci wa wb =
	foldr (.) id (zipWith setBit [op0, op1] $ wordToBits 64 op)
		. setBit ci bci
		. foldr (.) id (zipWith setBit a $ wordToBits 64 wa)
		. foldr (.) id (zipWith setBit b $ wordToBits 64 wb)

peekAlu :: ((IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) -> Circuit -> (Word64, Bit)
peekAlu (_, _, _, _, r, co) cct = (bitsToWord $ (`peekOWire` cct) <$> r, peekOWire co cct)

alu1_s :: CircuitBuilder (IWire, (IWire, IWire), IWire, IWire, IWire, OWire, OWire)
alu1_s = do
	(bin, bout) <- idGate
	(ni, no) <- notGate
	(m0, m1, si, mo) <- mux21
	(op, ci, a, b, r, co) <- alu1
	connectWire bout ni
	connectWire bout m0
	connectWire no m1
	connectWire mo b
	return (si, op, ci, a, bin, r, co)

alu_s :: Int -> CircuitBuilder (IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire)
alu_s n | n < 1 = error "Oops!"
alu_s 1 = do
	(biv, op, ci, a, b, r, co) <- alu1_s
	return (biv, op, ci, [a], [b], [r], co)
alu_s n = do
	(bivin, bivout) <- idGate
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	(biv1, (op10, op11), ci1, a1, b1, r1, co1) <- alu1_s
	(bivn, (opn0, opn1), cin, an, bn, rn, con) <- alu_s $ n - 1
	connectWire bivout biv1
	connectWire bivout bivn
	connectWire op0out op10
	connectWire op1out op11
	connectWire op0out opn0
	connectWire op1out opn1
	connectWire co1 cin
	return (bivin, (op0in, op1in), ci1, a1 : an, b1 : bn, r1 : rn, con)

setAlu_s :: (IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) ->
	Bit -> Word64 -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setAlu_s (biv, (op0, op1), ci, a, b, _, _) bbiv op bci wa wb = setBit biv bbiv
	. foldr (.) id (zipWith setBit [op0, op1] $ wordToBits 64 op)
	. setBit ci bci
	. foldr (.) id (zipWith setBit a $ wordToBits 64 wa)
	. foldr (.) id (zipWith setBit b $ wordToBits 64 wb)

peekAlu_s :: (IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) -> Circuit -> (Word64, Bit)
peekAlu_s (_, _, _, _, _, r, co) cct = (bitsToWord $ (`peekOWire` cct) <$> r, peekOWire co cct)

alu1_nor :: CircuitBuilder (IWire, IWire, (IWire, IWire), IWire, IWire, IWire, OWire, OWire)
alu1_nor = do
	(ain, aout) <- idGate
	(ni, no) <- notGate
	(m0, m1, si, mo) <- mux21
	(biv, op, ci, a, b, r, co) <- alu1_s
	connectWire aout ni
	connectWire aout m0
	connectWire no m1
	connectWire mo a
	return (si, biv, op, ci, ain, b, r, co)

alu_nor :: Int -> CircuitBuilder (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire)
alu_nor n | n < 1 = error "Oops!"
alu_nor 1 = do
	(aiv, biv, op, ci, a, b, r, co) <- alu1_nor
	return (aiv, biv, op, ci, [a], [b], [r], co)
alu_nor n = do
	(aivin, aivout) <- idGate
	(bivin, bivout) <- idGate
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	(aiv1, biv1, (op10, op11), ci1, a1, b1, r1, co1) <- alu1_nor
	(aivn, bivn, (opn0, opn1), cin, an, bn, rn, con) <- alu_nor $ n - 1
	connectWire aivout aiv1
	connectWire aivout aivn
	connectWire bivout biv1
	connectWire bivout bivn
	connectWire op0out op10
	connectWire op1out op11
	connectWire op0out opn0
	connectWire op1out opn1
	connectWire co1 cin
	return (aivin, bivin, (op0in, op1in), ci1, a1 : an, b1 : bn, r1 : rn, con)

setAlu_nor :: (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) ->
	Bit -> Bit -> Word64 -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setAlu_nor (aiv, biv, (op0, op1), ci, a, b, _, _) baiv bbiv op bci wa wb = setBit aiv baiv . setBit biv bbiv
	. foldr (.) id (zipWith setBit [op0, op1] $ wordToBits 64 op)
	. setBit ci bci
	. foldr (.) id (zipWith setBit a $ wordToBits 64 wa)
	. foldr (.) id (zipWith setBit b $ wordToBits 64 wb)

peekAlu_nor :: (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) -> Circuit -> (Word64, Bit)
peekAlu_nor (_, _, _, _, _, _, r, co) cct = (bitsToWord $ (`peekOWire` cct) <$> r, peekOWire co cct)

alu1_slt :: CircuitBuilder (IWire, IWire, (IWire, IWire), IWire, IWire, IWire, IWire, OWire, OWire)
alu1_slt = do
	(ain, a) <- idGate
	(bin, b) <- idGate
	(lin, lo) <- idGate
	(ai1, ai2, ao) <- andGate
	(oi1, oi2, oo) <- orGate
	(ci, adi1, adi2, s, co) <- adder1
	(op, mis, mo) <- mux_1 4
	case (op, mis) of
		([o0, o1], [mi0, mi1, mi2, mi3]) -> do
			connectWire a ai1
			connectWire b ai2
			connectWire a oi1
			connectWire b oi2
			connectWire a adi1
			connectWire b adi2
			connectWire ao mi0
			connectWire oo mi1
			connectWire s mi2
			connectWire lo mi3
			(aiv, ain') <- flipIf ain
			(biv, bin') <- flipIf bin
			return (aiv, biv, (o0, o1), ci, ain', bin', lin, mo, co)
		_ -> error "Oops!"

alu1_ms :: CircuitBuilder (IWire, IWire, (IWire, IWire), IWire, IWire, IWire, IWire, OWire, OWire, OWire)
alu1_ms = do
	(ain, a) <- idGate
	(bin, b) <- idGate
	(lin, lo) <- idGate
	(ai1, ai2, ao) <- andGate
	(oi1, oi2, oo) <- orGate
	(ci, adi1, adi2, s, _co) <- adder1
	(op, mis, mo) <- mux_1 4
	case (op, mis) of
		([o0, o1], [mi0, mi1, mi2, mi3]) -> do
			connectWire a ai1
			connectWire b ai2
			connectWire a oi1
			connectWire b oi2
			connectWire a adi1
			connectWire b adi2
			connectWire ao mi0
			connectWire oo mi1
			connectWire s mi2
			connectWire lo mi3
			(aiv, ain') <- flipIf ain
			(biv, bin') <- flipIf bin
			(stin, st) <- idGate
			(ovflin, ovfl) <- idGate
			connectWire s stin
			detectOverflow a b s ovflin
			return (aiv, biv, (o0, o1), ci, ain', bin', lin, mo, st, ovfl)
		_ -> error "Oops!"

flipIf :: IWire -> CircuitBuilder (IWire, IWire)
flipIf x = do
	(i, o) <- idGate
	(ni, no) <- notGate
	(a, b, si, mo) <- mux21
	connectWire o ni
	connectWire o a
	connectWire no b
	connectWire mo x
	return (si, i)

detectOverflow :: OWire -> OWire -> OWire -> IWire -> CircuitBuilder ()
detectOverflow a b s ovfl = do
	(xi1, xi2, xo) <- xorGate
	(ni, no) <- notGate
	(yi1, yi2, yo) <- xorGate
	(ai1, ai2, ao) <- andGate
	connectWire a xi1
	connectWire b xi2
	connectWire xo ni
	connectWire b yi1
	connectWire s yi2
	connectWire no ai1
	connectWire yo ai2
	connectWire ao ovfl

alu_slt :: Int -> CircuitBuilder (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire, IWire)
alu_slt n | n < 1 = error "Oops!"
alu_slt 1 = do
	(aiv, biv, op, ci, a, b, l, r, co) <- alu1_slt
	return (aiv, biv, op, ci, [a], [b], [r], co, l)
alu_slt n = do
	(aivin, aivout) <- idGate
	(bivin, bivout) <- idGate
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	(aiv1, biv1, (op10, op11), ci1, a1, b1, l1, r1, co1) <- alu1_slt
	(aivn, bivn, (opn0, opn1), cin, an, bn, rn, con, _ln) <- alu_slt $ n - 1
	connectWire aivout aiv1
	connectWire aivout aivn
	connectWire bivout biv1
	connectWire bivout bivn
	connectWire op0out op10
	connectWire op1out op11
	connectWire op0out opn0
	connectWire op1out opn1
	connectWire co1 cin
	return (aivin, bivin, (op0in, op1in), ci1, a1 : an, b1 : bn, r1 : rn, con, l1)

alu_ms :: Int -> CircuitBuilder (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire)
alu_ms n = do
	(aivin, aivout) <- idGate
	(bivin, bivout) <- idGate
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	(aivn, bivn, (opn0, opn1), cin, an, bn, rn, con, l1) <- alu_slt $ n - 1
	(aiv1, biv1, (op10, op11), ci1, a1, b1, _l1, r1, st, ovfl) <- alu1_ms
	connectWire aivout aivn
	connectWire aivout aiv1
	connectWire bivout bivn
	connectWire bivout biv1
	connectWire op0out opn0
	connectWire op0out op10
	connectWire op1out opn1
	connectWire op1out op11
	connectWire con ci1
	connectWire st l1
	return (aivin, bivin, (op0in, op1in), cin, an ++ [a1], bn ++ [b1], rn ++ [r1],  ovfl)

setAlu_ms :: (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) ->
	Bit -> Bit -> Word64 -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setAlu_ms (aiv, biv, (op0, op1), ci, a, b, _, _) baiv bbiv op bci wa wb = setBit aiv baiv . setBit biv bbiv
	. foldr (.) id (zipWith setBit [op0, op1] $ wordToBits 64 op)
	. setBit ci bci
	. foldr (.) id (zipWith setBit a $ wordToBits 64 wa)
	. foldr (.) id (zipWith setBit b $ wordToBits 64 wb)

peekAlu_ms :: (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) -> Circuit -> (Word64, Bit)
peekAlu_ms (_, _, _, _, _, _, r, ovfl) cct = (bitsToWord $ (`peekOWire` cct) <$> r, peekOWire ovfl cct)

peekAlu_ms' :: (IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire) -> Circuit -> ([Bit], Bit)
peekAlu_ms' (_, _, _, _, _, _, r, ovfl) cct = ((`peekOWire` cct) <$> r, peekOWire ovfl cct)
