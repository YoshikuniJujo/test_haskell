{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Word

import Circuit
import Tools

nandGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
nandGate ln pi1 pi2 po = do
	(a, b, o) <- andGate ln pi1 pi2 po
	(ni, no) <- notGate ln po po
	connectWire (o, ln, po) (ni, ln, po)
	return (a, b, no)

norGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
norGate ln pi1 pi2 po = do
	(a, b, o) <- orGate ln pi1 pi2 po
	(ni, no) <- notGate ln po po
	connectWire (o, ln, po) (ni, ln, po)
	return (a, b, no)

norGate0 :: CircuitBuilder (IWire, IWire, OWire)
norGate0 = norGate 1 0 0 0

norGate64 :: CircuitBuilder (IWire, IWire, OWire)
norGate64 = norGate 64 0 0 0

xorGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
xorGate ln pi1 pi2 po = do
	(ain, aout) <- idGate ln pi1 pi1
	(bin, bout) <- idGate ln pi2 pi2
	(oa, ob, oo) <- orGate ln pi1 pi2 po
	(naa, nab, nao) <- nandGate ln pi1 pi2 po
	(aa, ab, ao) <- andGate ln po po po
	connectWire (aout, ln, pi1) (oa, ln, pi1)
	connectWire (bout, ln, pi2) (ob, ln, pi2)
	connectWire (aout, ln, pi1) (naa, ln, pi1)
	connectWire (bout, ln, pi2) (nab, ln, pi2)
	connectWire (oo, ln, po) (aa, ln, po)
	connectWire (nao, ln, po) (ab, ln, po)
	return (ain, bin, ao)

orGate3 :: BitLen -> BitPosIn -> CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate3 l p = first listToTuple3 <$> multiple orGate 3 l p

orGateAllBits64 :: CircuitBuilder (IWire, OWire)
orGateAllBits64 = do
	(rin, rout) <- idGate0
	(abi, abo) <- orGateAllBits 32
	connectWire0 abo rin
	return (abi, rout)

orGateAllBits :: Word8 -> CircuitBuilder (IWire, OWire)
orGateAllBits 0 = idGate64
orGateAllBits n = do
	(abi, abo) <- orGateAllBits (n `div` 2)
	(sli, slo) <- orGateSlide n
	connectWire64 abo sli
	return (abi, slo)

orGateSlide :: Word8 -> CircuitBuilder (IWire, OWire)
orGateSlide n = do
	(bsin, bsout) <- idGate64
	(oa, ob, oo) <- orGate (64 - n) 0 n 0
	connectWire64 bsout `mapM_` [oa, ob]
	return (bsin, oo)

xorGate3 :: BitLen -> BitPosIn -> CircuitBuilder ((IWire, IWire, IWire), OWire)
xorGate3 l p = first listToTuple3 <$> multiple xorGate 3 l p

multiple ::
	(BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
			CircuitBuilder (IWire, IWire, OWire)) ->
		Word16 -> BitLen -> BitPosIn -> CircuitBuilder ([IWire], OWire)
multiple _ n _ _ | n < 1 = error "Oops!"
multiple _ 1 l p = first (: []) <$> idGate l p p
multiple g 2 l p = (\(i1, i2, o) -> ([i1, i2], o)) <$> g l p p p
multiple g n l p = do
	(is1, o1) <- multiple g (n `div` 2) l p
	(is2, o2) <- multiple g (n - n `div` 2) l p
	(i1, i2, o) <- g l p p p
	connectWire (o1, l, p) (i1, l, p)
	connectWire (o2, l, p) (i2, l, p)
	return (is1 ++ is2, o)

mux2 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2 = do
	(sl, is, o) <- multiplexer 2
	let	(i0, i1) = listToTuple2 is
	return (sl, i0, i1, o)

multiplexer :: Word16 -> CircuitBuilder (IWire, [IWire], OWire)
multiplexer n = do
	(slin, sout) <- idGate 64 0 0
	(dins, douts) <- decoder n
	for_ (zip [0 ..] dins)
		$ \(i, din) -> connectWire (sout, 1, i) (din, 1, 0)
	(as, bs, os) <- unzip3 <$> fromIntegral n `replicateM` andGate64
	(ois, oo) <- multiOrGate n 64 0
	zipWithM_ connectWire0_64 douts as
	zipWithM_ connectWire64 os ois
	return (slin, bs, oo)

decoder :: Word16 -> CircuitBuilder ([IWire], [OWire])
decoder n = do
	(is, ois) <- unzip <$> fromIntegral m `replicateM` idGate0
	(ias, oas) <- unzip <$> fromIntegral n `replicateM` multiAndGate m 1 0
	zipWithM_ ((sequence_ .)
		. flip (zipWith3 id) ois) (binary (inverse, obverse) m) ias
	return (is, oas)
	where m = log2 n

decoder' :: Word8 -> CircuitBuilder (IWire, OWire)
decoder' n = do
	(iin, iout) <- idGate 64 0 0
	(oin, oout) <- idGate 64 0 0
	(is, os) <- decoder $ fromIntegral n
	for_ (zip [0 .. ] is) $ \(i, ip) -> connectWire (iout, 1, i) (ip, 1, 0)
	for_ (zip [0 .. ] os) $ \(i, op) -> connectWire (op, 1, 0) (oin, 1, i)
	return (iin, oout)

decoder'' :: Word8 -> CircuitBuilder (IWire, [OWire])
decoder'' n = do
	(iin, iout) <- idGate64
	(is, os) <- decoder $ fromIntegral n
	for_ (zip [0 ..] is) $ \(i, ip) -> connectWire (iout, 1, i) (ip, 1, 0)
	return (iin, os)

multiAndGate, multiOrGate ::
	Word16 -> BitLen -> BitPosIn -> CircuitBuilder ([IWire], OWire)
multiAndGate = multiple andGate
multiOrGate = multiple orGate

inverse, obverse :: OWire -> IWire -> CircuitBuilder ()
inverse o i = do
	(ni, no) <- notGate0
	zipWithM_ connectWire0 [o, no] [ni, i]
obverse = connectWire0

rslatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
rslatch = do
	(r, q_', q) <- norGate64
	(s, q', q_) <- norGate64
	zipWithM_ connectWire64 [q, q_] [q', q_']
	return (r, s, q, q_)

dlatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
dlatch = do
	(cin, cout) <- idGate0
	(din, dout) <- idGate64
	(d, nd) <- notGate64
	(c1, nd', r) <- andGate64
	(c2, d', s) <- andGate64
	(r', s', q, q_) <- rslatch
	connectWire (cout, 1, 0) `mapM_` [(c1, 64, 0), (c2, 64, 0)]
	connectWire64 dout `mapM_` [d, d']
	connectWire64 nd nd'
	zipWithM_ connectWire64 [r, s] [r', s']
	return (cin, din, q, q_)

dflipflop :: CircuitBuilder (IWire, IWire, OWire, OWire)
dflipflop = do
	(cin, cout) <- idGate0
	(c, nc) <- notGate0
	(mc, md, mq, _mq_) <- dlatch
	(sc, sd, sq, sq_) <- dlatch
	connectWire0 cout `mapM_` [c, mc]
	connectWire0 nc sc
	connectWire64 mq sd
	return (cin, md, sq, sq_)

invert :: CircuitBuilder (IWire, IWire, OWire)
invert = do
	(xin, xout) <- idGate64
	(nxin, nxout) <- notGate64
	(xinv, xnx, xout') <- multiplexer 2
	let	(x, nx) = listToTuple2 xnx
	zipWithM_ connectWire64 [xout, xout, nxout] [nxin, x, nx]
	return (xinv, xin, xout')
