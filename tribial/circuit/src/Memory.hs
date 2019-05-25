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
