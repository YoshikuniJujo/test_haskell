{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Word

import Circuit
import Tools

nand, norGate :: CircuitBuilder (IWire, IWire, OWire)
nand = do
	(a, b, ao) <- andGate
	(ni, no) <- notGate
	connectWire ao ni
	return (a, b, no)

norGate = do
	(a, b, oo) <- orGate
	(ni, no) <- notGate
	connectWire oo ni
	return (a, b, no)

xorGate, xnor :: CircuitBuilder (IWire, IWire, OWire)
xorGate = do
	((ain, aout), (bin, bout)) <- listToTuple2 <$> replicateM 2 idGate
	(a1, a2, ao) <- andGate
	(o1, o2, oo) <- orGate
	(ni, no) <- notGate
	(r1, r2, r) <- andGate
	zipWithM_ connectWire
		[aout, bout, ao, no, aout, bout, oo]
		[a1, a2, ni, r1, o1, o2, r2]
	return (ain, bin, r)

xnor = do
	((ain, aout), (bin, bout)) <- listToTuple2 <$> replicateM 2 idGate
	(a1, a2, ao) <- andGate
	(o1, o2, oo) <- orGate
	(ni, no) <- notGate
	(r1, r2, r) <- orGate
	zipWithM_ connectWire
		[aout, bout, ao, aout, bout, oo, no]
		[a1, a2, r1, o1, o2, ni, r2]
	return (ain, bin, r)

mux2 :: CircuitBuilder Wires31
mux2 = do
	(si, sout) <- idGate
	(s', nsout) <- notGate
	(a, ns', o1out) <- andGate
	(b, s'', o2out) <- andGate
	(o1, o2, r) <- orGate
	zipWithM_ connectWire
		[sout, nsout, o1out, sout, o2out]
		[s', ns', o1, s'', o2]
	return (si, a, b, r)

mux3 :: CircuitBuilder Wires51
mux3 = do
	(o0, o1, s0, s1, s2) <- dec23
	((a, s0', ao), (b, s1', bo), (c, s2', co)) <- 
		listToTuple3 <$> replicateM 3 andGate
	((ao', bo', co'), r) <- orGate3
	zipWithM_ connectWire
		[s0, ao, s1, bo, s2, co]
		[s0', ao', s1', bo', s2', co']
	return (o0, o1, a, b, c, r)

dec23 :: CircuitBuilder (IWire, IWire, OWire, OWire, OWire)
dec23 = do
	((b0in, b0out, rb0out), (b1in, b1out, rb1out)) <-
		listToTuple2 <$> replicateM 2 obrev
	((r01, r02, r0), (r11, r12, r1), (r21, r22, r2)) <-
		listToTuple3 <$> replicateM 3 andGate
	zipWithM_ connectWire
		[rb0out, rb1out, b0out, rb1out, rb0out, b1out]
		[r01, r02, r11, r12, r21, r22]
	return (b0in, b1in, r0, r1, r2)

mux4 :: CircuitBuilder Wires61
mux4 = do
	(o0, o1, s0, s1, s2, s3) <- dec24
	((a, s0', ao), (b, s1', bo), (c, s2', co), (e, s3', eo)) <-
		listToTuple4 <$> replicateM 4 andGate
	((ao', bo', co', eo'), r) <- orGate4
	zipWithM_ connectWire
		[s0, ao, s1, bo, s2, co, s3, eo]
		[s0', ao', s1', bo', s2', co', s3', eo']
	return (o0, o1, a, b, c, e, r)

dec24 :: CircuitBuilder (IWire, IWire, OWire, OWire, OWire, OWire)
dec24 = do
	((b0in, b0out, rb0out), (b1in, b1out, rb1out)) <-
		listToTuple2 <$> replicateM 2 obrev
	((r01, r02, r0), (r11, r12, r1), (r21, r22, r2), (r31, r32, r3)) <-
		listToTuple4 <$> replicateM 4 andGate
	zipWithM_ connectWire
		[rb0out, rb1out, b0out, rb1out, rb0out, b1out, b0out, b1out]
		[r01, r02, r11, r12, r21, r22, r31, r32]
	return (b0in, b1in, r0, r1, r2, r3)

orGate5, andGate5 :: CircuitBuilder ((IWire, IWire, IWire, IWire, IWire), OWire)
orGate5 = first listToTuple5 <$> multiOrGate 5
andGate5 = first listToTuple5 <$> multiAndGate 5

orGate4, andGate4 :: CircuitBuilder ((IWire, IWire, IWire, IWire), OWire)
orGate4 = first listToTuple4 <$> multiOrGate 4
andGate4 = first listToTuple4 <$> multiAndGate 4

orGate3, andGate3 :: CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate3 = first listToTuple3 <$> multiOrGate 3
andGate3 = first listToTuple3 <$> multiAndGate 3

multiOrGate, multiAndGate :: Word8 -> CircuitBuilder ([IWire], OWire)
multiOrGate = multiple orGate
multiAndGate = multiple andGate

multiple :: CircuitBuilder (IWire, IWire, OWire) ->
	Word8 -> CircuitBuilder ([IWire], OWire)
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

obrev :: CircuitBuilder (IWire, OWire, OWire)
obrev = do
	(i, ob) <- idGate
	(ob', rev) <- notGate
	connectWire ob ob'
	return (i, ob, rev)

flipIf :: IWire -> CircuitBuilder (IWire, IWire)
flipIf x = do
	(i, o) <- idGate
	(ni, no) <- notGate
	(si, a, b, mo) <- mux2
	connectWire o ni
	connectWire o a
	connectWire no b
	connectWire mo x
	return (si, i)

type MultiplexerWires = ([IWire], [[IWire]], [OWire])

multiplexer :: Word8 -> Word8 -> CircuitBuilder MultiplexerWires
multiplexer n r = do
	(num, s) <- decoder r
	(s', ds, rslt) <- selector n r
	zipWithM_ connectWire s s'
	return (num, ds, rslt)

setBits :: [IWire] -> [Bit] -> Circuit -> Circuit
setBits ws = flip (foldr $ uncurry setBit) . zip ws

setBitsMultiplexer ::
	MultiplexerWires -> Word64 -> [Word64] -> Circuit -> Circuit
setBitsMultiplexer (wn, wds, _) n ds = setBits wn (wordToBits 64 n)
	. foldr (.) id (zipWith setBits wds $ wordToBits 64 <$> ds)

peekBitsMultiplexer :: MultiplexerWires -> Circuit -> Word64
peekBitsMultiplexer (_, _, wr) cct = bitsToWord $ (`peekOWire` cct) <$> wr

selector :: Word8 -> Word8 -> CircuitBuilder ([IWire], [[IWire]], [OWire])
selector n r = do
	(sins, souts) <- unzip <$> fromIntegral r `replicateM` idGate
	(sss, iss, os) <- unzip3 <$> fromIntegral n `replicateM` selector1 r
	mapM_ (zipWithM_ connectWire souts) sss
	return (sins, transpose iss, os)

selector1 :: Word8 -> CircuitBuilder ([IWire], [IWire], OWire)
selector1 r = do
	(ss, is, os) <- unzip3 <$> fromIntegral r `replicateM` andGate
	(os', rslt) <- multiOrGate r
	zipWithM_ connectWire os os'
	return (ss, is, rslt)

decoder :: Word8 -> CircuitBuilder ([IWire], [OWire])
decoder n = do
	(is, ois) <- unzip <$> fromIntegral m `replicateM` idGate
	(ias, oas) <- unzip <$> fromIntegral n `replicateM` multiAndGate m
	zipWithM_ ((sequence_ .) . flip (zipWith3 id) ois) (binary (invert, obvert) m) ias
	return (is, oas)
	where m = log2 n

binary :: (a, a) -> Word8 -> [[a]]
binary _ n | n < 1 = [[]]
binary (o, i) n = binary (o, i) (n - 1) >>= (<$> [(o :), (i :)]) . flip ($)

invert, obvert :: OWire -> IWire -> CircuitBuilder ()
invert o i = do
	(ni, no) <- notGate
	connectWire o ni
	connectWire no i
obvert = connectWire

log2 :: Integral n => n -> n
log2 i = l2 0
	where l2 j
		| j < 0 = error "bad"
		| 2 ^ j >= i = j
		| otherwise = l2 $ j + 1
