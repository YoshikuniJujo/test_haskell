{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Control.Monad
import Data.Word

import Circuit

norGate :: CircuitBuilder (IWire, IWire, OWire)
norGate = do
	(i1, i2, o) <- orGate
	(ni, no) <- notGate
	connectWire o ni
	return (i1, i2, no)

decoder :: Word16 -> CircuitBuilder ([IWire], [OWire])
decoder n = do
	(is, ois) <- unzip <$> fromIntegral m `replicateM` idGate
	(ias, oas) <- unzip <$> fromIntegral n `replicateM` multiAndGate m
	zipWithM_ ((sequence_ .) . flip (zipWith3 id) ois) (binary (inverse, obverse) m) ias
	return (is, oas)
	where m = log2 n

mux3_1 :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, OWire)
mux3_1 = do
	(sl, ds, r) <- multiplexer1 3
	return ((sl !! 0, sl !! 1), ds !! 0, ds !! 1, ds !! 2, r)

mux4_1 :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, IWire, OWire)
mux4_1 = do
	(sl, ds, r) <- multiplexer1 4
	return ((sl !! 0, sl !! 1), ds !! 0, ds !! 1, ds !! 2, ds !! 3, r)

multiplexer1 :: Word16 -> CircuitBuilder ([IWire], [IWire], OWire)
multiplexer1 n = do
	(sl, dc) <- decoder n
	(adi1s, adi2s, ados) <- unzip3 <$> fromIntegral n `replicateM` andGate
	(ois, oo) <- multiOrGate n
	zipWithM_ connectWire dc adi1s
	zipWithM_ connectWire ados ois
	return (sl, adi2s, oo)

binary :: (a, a) -> Word16 -> [[a]]
binary _ n | n < 1 = [[]]
binary (o, i) n = binary (o, i) (n - 1) >>= (<$> [(o :), (i :)]) . flip ($)

inverse, obverse :: OWire -> IWire -> CircuitBuilder ()
inverse o i = do
	(ni, no) <- notGate
	zipWithM_ connectWire [o, no] [ni, i]
obverse = connectWire

log2 :: Integral n => n -> n
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

{-
asisUnless :: IWire -> CircuitBuilder (IWire, IWire)
asisUnless i = do
	(cin, cout) <- idGate
	(c, dt, dto) <- triGate
	(ni, no) <- notGate
	(mi, mo) <- idGate
	(nc, m, ai) <- triGate
	mapM_ (connectWire cout) [c, ni]
	connectWire dto mi
	connectWire no nc
	connectWire mo m
	connectWire ai mi
	connectWire mo i
	return (cin, dt)
	-}

asisUnless :: IWire -> CircuitBuilder (IWire, IWire)
asisUnless i = do
	(cin, cout) <- idGate
	(ni, no) <- notGate
	(idi, ido) <- idGate
	(mi, mo) <- idGate
	(nc, m, ai) <- triGate
	(dtin, dtout) <- delay 10
	(c, dti, dto) <- triGate
	connectWire cout ni
	connectWire no nc
	connectWire mo m
	connectWire ai mi
	connectWire mo i
	connectWire dto mi
	connectWire cout idi
	connectWire ido c
	connectWire dtout dti
	return (cin, dtin)

testAsisUnless :: CircuitBuilder (IWire, IWire, OWire)
testAsisUnless = do
	(win, wout) <- idGate
	(c, d) <- asisUnless win
	return (c, d, wout)

connectAndGate :: IWire -> CircuitBuilder (IWire, IWire)
connectAndGate i = do
	(ai1, ai2, ao) <- andGate
	connectWire ao i
	return (ai1, ai2)

strictGates1 :: Word8 -> CircuitBuilder (IWire, OWire) -> CircuitBuilder ([IWire], IWire, OWire)
strictGates1 n g0 = do
	(iin, iout) <- idGate
	(ad, dc) <- decoder (2 ^ n)
	(gis, gos) <- unzip <$> (2 ^ n) `replicateM` g0
--	(auc, aud) <- unzip <$> asisUnless `mapM` gis
	(auc, aud) <- unzip <$> connectAndGate `mapM` gis
	zipWithM_ connectWire dc auc
	mapM_ (connectWire iout) aud
	(tcs, tds, tos) <- unzip3 <$> (2 ^ n) `replicateM` triGate
	zipWithM_ connectWire dc tcs
	zipWithM_ connectWire gos tds
	(rin, rout) <- idGate
	mapM_ (`connectWire` rin) tos
	return (ad, iin, rout)

strictGates :: Word8 -> CircuitBuilder ([IWire], OWire) -> CircuitBuilder ([IWire], [IWire], OWire)
strictGates n g0 = do
	(iins, iouts) <- unzip <$> (2 ^ n) `replicateM` idGate
	(ad, dc) <- decoder (2 ^ n)
	(giss, gos) <- unzip <$> (2 ^ n) `replicateM` g0
	(aucs, auds) <- unzip <$> ((unzip <$>) . (asisUnless `mapM`)) `mapM` giss
	zipWithM_ (\dc1 -> mapM_ (connectWire dc1)) dc aucs
	mapM_ (zipWithM_ connectWire iouts) auds
	(tcs, tds, tos) <- unzip3 <$> (2 ^ n) `replicateM` triGate
	zipWithM_ connectWire dc tcs
	zipWithM_ connectWire gos tds
	(rin, rout) <- idGate
	mapM_ (`connectWire` rin) tos
	return (ad, iins, rout)

orGate3 :: CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate3 = do
	(a, b, ab) <- orGate
	(ab', c, r) <- orGate
	connectWire ab ab'
	return ((a, b, c), r)

nandGate :: CircuitBuilder (IWire, IWire, OWire)
nandGate = do
	(a1, a2, ao) <- andGate
	(ni, no) <- notGate
	connectWire ao ni
	return (a1, a2, no)

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
