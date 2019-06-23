{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List
import Data.Bool
import Data.Word

import Circuit
import Tools

import qualified Data.Bits as B

nandGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
nandGate ln pi1 pi2 po = do
	(a, b, o) <- andGate ln pi1 pi2 po
	(ni, no) <- notGate ln po po
	connectWire (o, ln, po) (ni, ln, po)
	return (a, b, no)

nandGate0 :: CircuitBuilder (IWire, IWire, OWire)
nandGate0 = nandGate 1 0 0 0

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

xorGate0 :: CircuitBuilder (IWire, IWire, OWire)
xorGate0 = xorGate 1 0 0 0

orGate3 :: BitLen -> BitPosIn -> CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate3 l p = first listToTuple3 <$> multiple orGate 3 l p

orGate0_3 :: CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate0_3 = orGate3 1 0

andGate3 :: BitLen -> BitPosIn -> CircuitBuilder ((IWire, IWire, IWire), OWire)
andGate3 l p = first listToTuple3 <$> multiple andGate 3 l p

andGate0_3 :: CircuitBuilder ((IWire, IWire, IWire), OWire)
andGate0_3 = andGate3 1 0

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
multiple _ n _ _ | n < 0 = error "Oops!"
multiple _ 0 l p = ([] ,) <$> constGate (Bits 0) l p p
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

mux3 :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
mux3 = do
	(sl, is, o) <- multiplexer 3
	let	(i0, i1, i2) = listToTuple3 is
	return (sl, i0, i1, i2, o)

mux4 :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire)
mux4 = do
	(sl, is, o) <- multiplexer 4
	let	(i0, i1, i2, i3) = listToTuple4 is
	return (sl, i0, i1, i2, i3, o)

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

inc :: Word8 -> OWire -> IWire -> CircuitBuilder OWire
inc 0 o i = do
	(ni, no) <- notGate0
	connectWire0 o ni
	connectWire0 no i
	(ci, co) <- idGate0
	connectWire0 o ci
	return co
inc n o i = do
	c <- inc (n - 1) o i
	(xa, xb, xo) <- xorGate0
	(aa, ab, ao) <- andGate0
	connectWire0 c xa
	connectWire0 c aa
	connectWire (o, 1, n) (xb, 1, 0)
	connectWire (o, 1, n) (ab, 1, 0)
	connectWire (xo, 1, 0) (i, 1, n)
	return ao

inc8 :: CircuitBuilder (IWire, OWire)
inc8 = do
	(iin, iout) <- idGate64
	(oin, oout) <- idGate64
	_ <- inc 7 iout oin
	return (iin, oout)

data Bit = O | I deriving (Show, Eq)

numToBits :: B.Bits n => Word8 -> n -> [Bit]
numToBits c n = map (bool O I . B.testBit n) [0 .. fromIntegral c - 1]

bitIndex :: (a, a) -> Bit -> a
bitIndex (x, _) O = x
bitIndex (_, y) I = y

frbk0 :: OWire -> CircuitBuilder (OWire, OWire)
frbk0 o = do
	(ni, no) <- notGate0
	connectWire0 o ni
	return (no, o)

allAnd0 :: [OWire] -> CircuitBuilder OWire
allAnd0 = allAnd 1 0

allAnd :: BitLen -> BitPosIn -> [OWire] -> CircuitBuilder OWire
allAnd ln ps os = do
	(as, o) <- multiAndGate (fromIntegral $ length os) ln ps
	zipWithM_ connectWire64 os as
	return o

allOr0 :: [OWire] -> CircuitBuilder OWire
allOr0 = allOr 1 0

allOr :: BitLen -> BitPosIn -> [OWire] -> CircuitBuilder OWire
allOr ln ps os = do
	(xs, o) <- multiOrGate (fromIntegral $ length os) ln ps
	zipWithM_ connectWire64 os xs
	return o

pla1 :: [OWire] -> [([Bit], Bit)] -> CircuitBuilder OWire
pla1 iouts tbl1 = do
	fbs <- frbk0 `mapM` iouts
	let	ws = map (zipWith bitIndex fbs) ons
	r <- allOr0 =<< mapM allAnd0 ws
	return r
	where
	ons = fst <$> filter ((== I) . snd) tbl1

sepSnd :: (a, [b]) -> [(a, b)]
sepSnd (x, ys) = zip (repeat x) ys

trSep :: [(a, [b])] -> [[(a, b)]]
trSep = transpose . map sepSnd

plaGen :: Word8 -> [([Bit], [Bit])] -> CircuitBuilder ([IWire], [OWire])
plaGen n_ tbl = do
	(iins, iouts) <- unzip <$> n `replicateM` idGate0
	rs <- pla1 iouts `mapM` trSep tbl
	return (iins, rs)
	where
	n = fromIntegral n_

connectWireOutToIns :: OWire -> [IWire] -> CircuitBuilder ()
connectWireOutToIns o is = zipWithM_ (\n i -> connectWire (o, 1, n) (i, 1, 0)) [0 ..] is

connectWireOutsToIn :: [OWire] -> IWire -> CircuitBuilder ()
connectWireOutsToIn os i = zipWithM_ (\n o -> connectWire (o, 1, 0) (i, 1, n)) [0 ..] os

pla8 :: [(Word8, Word8)] -> CircuitBuilder (IWire, OWire)
pla8 tbl_ = do
	(iin, iout) <- idGate64
	(iss, outs) <- plaGen 8 tbl
	(oin, oout) <- idGate64
	connectWireOutToIns iout iss
	connectWireOutsToIn outs oin
	return (iin, oout)
	where tbl = (numToBits 8 *** numToBits 8) <$> tbl_

pla8_16 :: [(Word8, Word16)] -> CircuitBuilder (IWire, OWire)
pla8_16 tbl_ = do
	(iin, iout) <- idGate64
	(iss, outs) <- plaGen 8 tbl
	(oin, oout) <- idGate64
	connectWireOutToIns iout iss
	connectWireOutsToIn outs oin
	return (iin, oout)
	where tbl = (numToBits 8 *** numToBits 16) <$> tbl_
