{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.Element where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Word

import Circuit

norGate :: CircuitBuilder (IWire, IWire, OWire)
norGate = do
	(i1, i2, o) <- orGate
	(ni, no) <- notGate
	connectWire o ni
	return (i1, i2, no)

type Wires31 = (IWire, IWire, IWire, OWire)

mux2 :: CircuitBuilder Wires31
mux2 = do
	(slin, slout) <- idGate
	(ni, no) <- notGate
	(ns, a, ao) <- andGate
	(s, b, bo) <- andGate
	(ai, bi, c) <- orGate
	zipWithM_ connectWire
		[slout, no, slout, ao, bo]
		[ni, ns, s, ai, bi]
	return (slin, a, b, c)

multiplexer :: Word8 -> Word16 -> CircuitBuilder ([IWire], [[IWire]], [OWire])
multiplexer m n = do
	(slin, slout) <- unzip <$> a `replicateM` idGate
	(sls, is, o) <- unzip3 <$> fromIntegral m `replicateM` multiplexer1 n
	zipWithM_ connectWire slout `mapM_` sls
	return (slin, transpose is, o)
	where a = log2 n

multiplexer1 :: Word16 -> CircuitBuilder ([IWire], [IWire], OWire)
multiplexer1 n = do
	(sl, dc) <- decoder n
	(adi1s, adi2s, ados) <- unzip3 <$> fromIntegral n `replicateM` andGate
	(ois, oo) <- multiOrGate n
	zipWithM_ connectWire dc adi1s
	zipWithM_ connectWire ados ois
	return (sl, adi2s, oo)

decoder :: Word16 -> CircuitBuilder ([IWire], [OWire])
decoder n = do
	(is, ois) <- unzip <$> fromIntegral m `replicateM` idGate
	(ias, oas) <- unzip <$> fromIntegral n `replicateM` multiAndGate m
	zipWithM_ ((sequence_ .) . flip (zipWith3 id) ois) (binary (inverse, obverse) m) ias
	return (is, oas)
	where m = log2 n

binary :: (a, a) -> Word16 -> [[a]]
binary _ n | n < 1 = [[]]
binary (o, i) n = binary (o, i) (n - 1) >>= (<$> [(o :), (i :)]) . flip ($)

inverse, obverse :: OWire -> IWire -> CircuitBuilder ()
inverse o i = do
	(ni, no) <- notGate
	zipWithM_ connectWire [o, no] [ni, i]
obverse = connectWire

log2 :: (Integral n, Integral m) => n -> m
log2 i = i2 0
	where i2 j
		| j < 0 = error "Oops!"
		| 2 ^ j >= i = j
		| otherwise = i2 $ j + 1

multiAndGate, multiOrGate :: Word16 -> CircuitBuilder ([IWire], OWire)
multiAndGate = multiple andGate
multiOrGate = multiple orGate

multiple :: CircuitBuilder (IWire, IWire, OWire) ->
	Word16 -> CircuitBuilder ([IWire], OWire)
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

xorGate :: CircuitBuilder (IWire, IWire, OWire)
xorGate = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(o1, o2, oo) <- orGate
	(na1, na2, nao) <- nandGate
	(a1, a2, ao) <- andGate
	zipWithM_ connectWire
		[aout, bout, aout, bout, oo, nao]
		[o1, o2, na1, na2, a1, a2]
	return (ain, bin, ao)

nandGate :: CircuitBuilder (IWire, IWire, OWire)
nandGate = do
	(a1, a2, ao) <- andGate
	(ni, no) <- notGate
	connectWire ao ni
	return (a1, a2, no)

mux4_1 :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, IWire, OWire)
mux4_1 = do
	(sl, ds, r) <- multiplexer1 4
	return ((sl !! 0, sl !! 1), ds !! 0, ds !! 1, ds !! 2, ds !! 3, r)
