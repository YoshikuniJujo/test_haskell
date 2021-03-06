{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Memory where

import Control.Monad
import Data.List
import Data.Word

import Circuit
import Element
import Clock
import Tools

srLatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
srLatch = do
	(r, q_', q) <- norGate
	(s, q', q_) <- norGate
	connectWire q q'
	connectWire q_ q_'
	return (r, s, q, q_)

dlatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
dlatch = do
	((cin, cout), (din, dout)) <- listToTuple2 <$> replicateM 2 idGate
	(ni, no) <- notGate
	(c1, nd, r) <- andGate
	(c2, d, s) <- andGate
	(r', q_', q) <- norGate
	(s', q', q_) <- norGate
	zipWithM_ connectWire
		[dout, cout, no, r, cout, dout, s, q_, q]
		[ni, c1, nd, r', c2, d, s', q_', q']
	return (cin, din, q, q_)

temporary :: Word8 -> Word8 -> CircuitBuilder OWire
temporary w dr = do
	(_, on) <- notGate
	(i1, i2, o) <- xorGate
	(wi, wo) <- delay w
	(dri, dro) <- delay $ w + dr
	zipWithM_ connectWire [on, on, wo, dro] [wi, dri, i1, i2]
	return o

temporaryData :: Word8 -> Word8 -> Word8 -> CircuitBuilder ([IWire], [OWire])
temporaryData n w dr = do
	t <- temporary w dr
	(a1s, ds, rs) <- unzip3 <$> replicateM (fromIntegral n) andGate
	mapM_ (connectWire t) a1s
	return (ds, rs)

testDlatch :: CircuitBuilder (OWire, OWire)
testDlatch = do
	c <- edge 10 100
	d <- temporary 1 30
	(dli, dlo) <- delay 5
	(c', d', q, q_) <- dlatch
	connectWire c dli
	connectWire dlo c'
	connectWire d d'
	return (q, q_)

dflipflop1 :: CircuitBuilder (IWire, IWire, OWire, OWire)
dflipflop1 = do
	(cin, cout) <- idGate
	(mc, md, mq, _mq_) <- dlatch
	(ni, no) <- notGate
	(sc, sd, sq, sq_) <- dlatch
	zipWithM_ connectWire [cout, cout, no, mq] [mc, ni, sc, sd]
	return (cin, md, sq, sq_)

testDflipflop1 :: CircuitBuilder (OWire, OWire)
testDflipflop1 = do
	c <- clock 100
	d <- temporary 80 40
	(c', d', q, q_) <- dflipflop1
	connectWire c c'
	connectWire d d'
	return (q, q_)

dflipflop :: Word8 -> CircuitBuilder (IWire, [IWire], [OWire], [OWire])
dflipflop n = do
	(cin, cout) <- idGate
	(cs, ds, qs, q_s) <- unzip4 <$> replicateM (fromIntegral n) dflipflop1
	mapM_ (connectWire cout) cs
	return (cin, ds, qs, q_s)

type TestDflipflopWires = ([IWire], [OWire], [OWire])

testDflipflop :: Word8 -> CircuitBuilder TestDflipflopWires
testDflipflop n = do
	c <- clock 100
	(din, dout) <- temporaryData n 80 40
	(c', ds', qs, q_s) <- dflipflop n
	connectWire c c'
	zipWithM_ connectWire dout ds'
	return (din, qs, q_s)

setBitsTestDflipflop :: TestDflipflopWires -> Word64 -> Circuit -> Circuit
setBitsTestDflipflop (ds, _, _) d =
	flip (foldr $ uncurry setBit) . zip ds $ wordToBits 64 d

peekBitsTestDflipflop :: TestDflipflopWires -> Circuit -> (Word64, Word64)
peekBitsTestDflipflop (_, qs, q_s) cct =
	(bitsToWord $ (`peekOWire` cct) <$> qs, bitsToWord $ (`peekOWire` cct) <$> q_s)

registerWrite :: Word8 -> Word8 -> CircuitBuilder (IWire, [IWire], [IWire], [[OWire]])
registerWrite nbit rnum = do
	(wrin, wrout) <- idGate
	(dins, douts) <- unzip <$> fromIntegral nbit `replicateM` idGate
	(rid, slct) <- decoder rnum
	(wrs, slct', cs) <- unzip3 <$> fromIntegral rnum `replicateM` andGate
	(cs', ds, qs, _q_s) <- unzip4 <$> fromIntegral rnum `replicateM` dflipflop nbit
	mapM_ (connectWire wrout) wrs
	zipWithM_ connectWire slct slct'
	zipWithM_ connectWire cs cs'
	mapM_ (zipWithM_ connectWire douts) ds
	return (wrin, rid, dins, qs)

type TestRegisterWriteWires = (IWire, [IWire], [IWire], OWire, [[OWire]])

testRegisterWrite :: Word8 -> Word8 -> CircuitBuilder TestRegisterWriteWires
testRegisterWrite nbit rnum = do
	c <- clock 100
	(a1, a2, ao) <- andGate
	(wr, rid, d, qs) <- registerWrite nbit rnum
	connectWire c a1
	connectWire ao wr
	return (a2, rid, d, c, qs)

setBitsTestRegisterWrite ::
	TestRegisterWriteWires -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setBitsTestRegisterWrite (wwr, wrid, wd, _, _) bwr rid d = setBit wwr bwr
	. setBits wrid (wordToBits 64 rid) . setBits wd (wordToBits 64 d)

peekBitsTestRegisterWrite :: TestRegisterWriteWires -> Circuit -> (Bit, [Word64])
peekBitsTestRegisterWrite (_, _, _, c, wrs) cct =
	(peekOWire c cct, (bitsToWord . ((`peekOWire` cct) <$>)) <$> wrs)

registerReadUnit :: Word8 -> Word8 -> [[OWire]] -> CircuitBuilder ([IWire], [OWire])
registerReadUnit nbit rnum qs = do
	(rid, rs, rtn) <- multiplexer nbit rnum
	zipWithM_ (zipWithM_ connectWire) qs rs
	return (rid, rtn)

registerFile :: Word8 -> Word8 -> CircuitBuilder (
	[IWire], [IWire], [IWire], [IWire],  IWire, [OWire], [OWire] )
registerFile nbit rnum = do
	(wr, rid, d, qs) <- registerWrite nbit rnum
	(rid1, rtn1) <- registerReadUnit nbit rnum qs
	(rid2, rtn2) <- registerReadUnit nbit rnum qs
	return (rid1, rid2, rid, d, wr, rtn1, rtn2)

type TestRegisterFileWires =
	([IWire], [IWire], [IWire], [IWire], IWire, OWire, [OWire], [OWire])

testRegisterFile :: Word8 -> Word8 -> CircuitBuilder TestRegisterFileWires
testRegisterFile nbit rnum = do
	c <- clock 50
	(a1, a2, ao) <- andGate
	(rid1, rid2, ridw, d, wr, rtn1, rtn2) <- registerFile nbit rnum
	connectWire c a1
	connectWire ao wr
	return (rid1, rid2, ridw, d, a2, c, rtn1, rtn2)

setBitsTestRegisterFile :: TestRegisterFileWires ->
	Word64 -> Word64 -> Word64 -> Word64 -> Bit -> Circuit -> Circuit
setBitsTestRegisterFile
		(wrid1, wrid2, wridw, wd, wwr, _, _, _) rid1 rid2 ridw d wr = do
	foldr (.) id (zipWith setBits
			[wrid1, wrid2, wridw, wd]
			(wordToBits 64 <$> [rid1, rid2, ridw, d]))
		. setBit wwr wr

peekBitsTestRegisterFile :: TestRegisterFileWires -> Circuit -> (Bit, Word64, Word64)
peekBitsTestRegisterFile (_, _, _, _, _, c, rtn1, rtn2) cct = (
	peekOWire c cct,
	bitsToWord $ (`peekOWire` cct) <$> rtn1,
	bitsToWord $ (`peekOWire` cct) <$> rtn2 )
