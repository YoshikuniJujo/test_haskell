{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryCircuit where

import Control.Monad
import Data.Word
import Data.Int

import Circuit
import Element
import Clock
import Memory
import Alu
import Tools

decode4 :: CircuitBuilder (IWire, OWire)
decode4 = do
	(iin, iout) <- idGate 2 0 0
	(rin, rout) <- idGate 4 0 0
	(ni0, no0) <- notGate 1 0 0
	(ni1, no1) <- notGate 1 0 0
	(a0, b0, o0) <- andGate 1 0 0 0
	(a1, b1, o1) <- andGate 1 0 0 0
	(a2, b2, o2) <- andGate 1 0 0 0
	(a3, b3, o3) <- andGate 1 0 0 0
	connectWire (iout, 1, 0) (ni0, 1, 0)
	connectWire (iout, 1, 1) (ni1, 1, 0)
	connectWire (no0, 1, 0) (a0, 1, 0)
	connectWire (no1, 1, 0) (b0, 1, 0)
	connectWire (iout, 1, 0) (a1, 1, 0)
	connectWire (no1, 1, 0) (b1, 1, 0)
	connectWire (no0, 1, 0) (a2, 1, 0)
	connectWire (iout, 1, 1) (b2, 1, 0)
	connectWire (iout, 1, 0) (a3, 1, 0)
	connectWire (iout, 1, 1) (b3, 1, 0)
	connectWire (o0, 1, 0) (rin, 1, 0)
	connectWire (o1, 1, 0) (rin, 1, 1)
	connectWire (o2, 1, 0) (rin, 1, 2)
	connectWire (o3, 1, 0) (rin, 1, 3)
	return (iin, rout)

mux2_1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2_1 = do
	(si, sout) <- idGate0
	(ni, no) <- notGate0
	(aa1, ab1, ao1) <- andGate0
	(aa2, ab2, ao2) <- andGate0
	(oa, ob, oo) <- orGate0
	connectWire0 sout ni
	connectWire0 no aa1
	connectWire0 sout aa2
	connectWire0 ao1 oa
	connectWire0 ao2 ob
	return (si, ab1, ab2, oo)

mux2_64 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2_64 = do
	(si, sout) <- idGate0
	(ni, no) <- notGate0
	connectWire0 sout ni
	(aa1, ab1, ao1) <- andGate64
	(aa2, ab2, ao2) <- andGate64
	(oa, ob, oo) <- orGate64
	connectWire0_64 no aa1
	connectWire0_64 sout aa2
	connectWire64 ao1 oa
	connectWire64 ao2 ob
	return (si, ab1, ab2, oo)

sum1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
sum1 = do
	((ci, a, b), s) <- xorGate3 1 0
	return (ci, a, b, s)

carry1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
carry1 = do
	(ciin, ciout) <- idGate0
	(ain, aout) <- idGate0
	(bin, bout) <- idGate0
	(a1, b1, i1) <- andGate0
	(c2, b2, i2) <- andGate0
	(c3, a3, i3) <- andGate0
	((o1, o2, o3), co) <- orGate3 1 0
	zipWithM_ connectWire0
		[aout, bout, ciout, bout, ciout, aout]
		[a1, b1, c2, b2, c3, a3]
	zipWithM_ connectWire0 [i1, i2, i3] [o1, o2, o3]
	return (ciin, ain, bin, co)

carry64 :: CircuitBuilder (IWire, IWire, IWire, OWire)
carry64 = do
	(ciin, ciout) <- idGate64
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(a1, b1, i1) <- andGate64
	(c2, b2, i2) <- andGate64
	(c3, a3, i3) <- andGate64
	((o1, o2, o3), co) <- orGate3 64 0
	zipWithM_ connectWire64
		[aout, bout, ciout, bout, ciout, aout]
		[a1, b1, c2, b2, c3, a3]
	zipWithM_ connectWire64 [i1, i2, i3] [o1, o2, o3]
	return (ciin, ain, bin, co)

alu_ao1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
alu_ao1 = do
	(ain, aout) <- idGate0
	(bin, bout) <- idGate0
	(aa, ab, ao) <- andGate0
	(oa, ob, oo) <- orGate0
	(op, ms, r) <- multiplexer 2
	let	(m0, m1) = listToTuple2 ms
	zipWithM_ connectWire0
		[aout, bout, aout, bout, ao, oo]
		[aa, ab, oa, ob, m0, m1]
	return (op, ain, bin, r)

adder1 :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
adder1 = do
	(ciin, ciout) <- idGate0
	(ain, aout) <- idGate0
	(bin, bout) <- idGate0
	(ci, a, b, s) <- sum1
	(ci', a', b', co) <- carry1
	zipWithM_ connectWire0
		[ciout, aout, bout, ciout, aout, bout]
		[ci, a, b, ci', a', b']
	return (ciin, ain, bin, s, co)

adder64 :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
adder64 = do
	(ciin, ciout) <- idGate0
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(coin, coout) <- idGate0
	(ci, a, b, s) <- sum64
	(ci', a', b', co) <- carry64
	zipWithM_ connectWire64 [aout, bout, aout, bout] [a, b, a', b']
	connectWire0 ciout `mapM_` [ci, ci']
	connectWire (co, 63, 0) (ci, 63, 1)
	connectWire (co, 63, 0) (ci', 63, 1)
	connectWire (co, 1, 63) (coin, 1, 0)
	return (ciin, ain, bin, s, coout)

alu_aos1 :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire, OWire)
alu_aos1 = do
	(ain, aout) <- idGate0
	(bin, bout) <- idGate0
	(aa, ab, ao) <- andGate0
	(oa, ob, oo) <- orGate0
	(ci, sa, sb, ss, co) <- adder1
	(op, ms, r) <- multiplexer 3
	let	(m0, m1, m2) = listToTuple3 ms
	zipWithM_ connectWire0
		[aout, bout, aout, bout, aout, bout, ao, oo, ss]
		[aa, ab, oa, ob, sa, sb, m0, m1, m2]
	return (op, ci, ain, bin, r, co)

alu_aos64 :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire, OWire)
alu_aos64 = do
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(nbin, nbout) <- notGate64
	(binv, bnb, bout') <- multiplexer 2
	let	(b, nb) = listToTuple2 bnb
	zipWithM_ connectWire64 [bout, bout, nbout] [nbin, b, nb]
	(aa, ab, ao) <- andGate64
	(oa, ob, oo) <- orGate64
	(ci, sa, sb, ss, co) <- adder64
	(op, ms, r) <- multiplexer 3
	let	(m0, m1, m2) = listToTuple3 ms
	zipWithM_ connectWire64
		[aout, bout', aout, bout', aout, bout', ao, oo, ss]
		[aa, ab, oa, ob, sa, sb, m0, m1, m2]
	return (binv, op, ci, ain, bin, r, co)

alu_aos64' :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire, OWire)
alu_aos64' = do
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(nbin, nbout) <- notGate64
	(binv, bnb, bout') <- multiplexer 2
	let	(b, nb) = listToTuple2 bnb
	zipWithM_ connectWire64 [bout, bout, nbout] [nbin, b, nb]
	(aa, ab, ao) <- andGate64
	(oa, ob, oo) <- orGate64
	(ci, sa, sb, ss, _slt, ovfl) <- adder64'
	(op, ms, r) <- multiplexer 3
	let	(m0, m1, m2) = listToTuple3 ms
	zipWithM_ connectWire64
		[aout, bout', aout, bout', aout, bout', ao, oo, ss]
		[aa, ab, oa, ob, sa, sb, m0, m1, m2]
	return (binv, op, ci, ain, bin, r, ovfl)

mkSampleAluAos64 :: CircuitBuilder
	(IWire, IWire, IWire, IWire, IWire, OWire, OWire) -> [Word64]
mkSampleAluAos64 u = let
	((binv, op, ci, a, b, r, _co), cct) = makeCircuit u
	cct0 = setBits binv (Bits 1) . setBits op (Bits 2) . setBits ci (Bits 1)
		. setBits a (Bits 987654321) $ setBits b (Bits 123456789) cct in
	bitsToWord . peekOWire r <$> iterate step cct0

alu_ :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, IWire, OWire, OWire)
alu_ = do
	(ainv, ain, aout) <- invert
	(binv, bin, bout) <- invert
	(aa, ab, ao) <- andGate64
	(oa, ob, oo) <- orGate64
	(ci, sa, sb, ss, _slt, ovfl) <- adder64'
	(op, ms, r) <- multiplexer 3
	let	(m0, m1, m2) = listToTuple3 ms
	zipWithM_ connectWire64
		[aout, bout, aout, bout, aout, bout, ao, oo, ss]
		[aa, ab, oa, ob, sa, sb, m0, m1, m2]
	return (ainv, binv, op, ci, ain, bin, r, ovfl)

setRiscvAlu :: RiscvAluWires -> Word64 -> Word64 -> Word64 -> Circuit -> Circuit
setRiscvAlu (wop, wa, wb, _, _, _) op a b = setBits wop (wordToBits op)
	. setBits wa (wordToBits a) . setBits wb (wordToBits b)

peekRiscvAlu :: RiscvAluWires -> Circuit -> (Int64, Word64, Word64)
peekRiscvAlu (_, _, _, r, z, ovfl) = (,,)
	<$> fromIntegral . bitsToWord . peekOWire r
	<*>  bitsToWord . peekOWire z <*> bitsToWord . peekOWire ovfl

-- overflow = carry in `xor` carry out
-- less than = sum (sign) `xor` overflow

testDelay :: CircuitBuilder (IWire, IWire, OWire)
testDelay = do
	(a, b, o) <- xorGate 64 0 0 0
	delay b 15
	return (a, b, o)

simpleClock :: CircuitBuilder (IWire, OWire)
simpleClock = do
	(i, o) <- notGate0
	connectWire0 o i
	delay i 15
	return (i, o)

rslatch1 :: CircuitBuilder (IWire, IWire, OWire, OWire)
rslatch1 = do
	(r, q_', q) <- norGate0
	(s, q', q_) <- norGate0
	zipWithM_ connectWire0 [q, q_] [q', q_']
	return (r, s, q, q_)

longpush :: Word8 -> IWire -> Bits -> Circuit -> Circuit
longpush 0 _ _ cct = cct
longpush n iw bs cct = step . setBits iw bs $ longpush (n - 1) iw bs cct

dlatch1 :: CircuitBuilder (IWire, IWire, OWire, OWire)
dlatch1 = do
	(cin, cout) <- idGate0
	(din, dout) <- idGate0
	(d, nd) <- notGate0
	(c1, nd', r) <- andGate0
	(c2, d', s) <- andGate0
	(r', s', q, q_) <- rslatch1
	connectWire0 cout `mapM_` [c1, c2]
	connectWire0 dout `mapM_` [d, d']
	connectWire0 nd nd'
	zipWithM_ connectWire0 [r, s] [r', s']
	return (cin, din, q, q_)

dflipflop1 :: CircuitBuilder (IWire, IWire, OWire, OWire)
dflipflop1 = do
	(cin, cout) <- idGate0
	(c, nc) <- notGate0
	(mc, md, mq, _mq_) <- dlatch1
	(sc, sd, sq, sq_) <- dlatch1
	connectWire0 cout `mapM_` [c, mc]
	zipWithM_ connectWire0 [nc, mq] [sc, sd]
	return (cin, md, sq, sq_)

clocked :: Word8 -> Bits -> CircuitBuilder OWire
clocked n bs = do
	(_ci, co) <- clockGen n
	d <- constGate bs 64 0 0
	(a, b, r) <- andGate64
	connectWire (co, 1, 0) (a, 64, 0)
	connectWire64 d b
	return r

testDflipflop1 :: CircuitBuilder (OWire, OWire, OWire)
testDflipflop1 = do
	(c, d, q, _q_) <- dflipflop1
	(_, ccl) <- clockGen 15
	(_, dcl) <- clockGen 25
	connectWire0 ccl c
	connectWire0 dcl d
	return (ccl, dcl, q)

testDflipflop :: CircuitBuilder (OWire, OWire, OWire)
testDflipflop = do
	(c, d, q, _q_) <- dflipflop
	(_, ccl) <- clockGen 15
	dcl <- clocked 25 $ Bits 1234567891
	connectWire0 ccl c
	connectWire64 dcl d
	return (ccl, dcl, q)

testDlatch :: CircuitBuilder (OWire, OWire, OWire)
testDlatch = do
	(c, d, q, _q_) <- dlatch
	(_, ccl) <- clockGen 15
	dcl <- clocked 25 $ Bits 1234567891
	connectWire0 ccl c
	connectWire64 dcl d
	return (ccl, dcl, q)

peekOWire3 :: OWire -> OWire -> OWire -> Circuit -> (Word64, Word64, Word64)
peekOWire3 o1 o2 o3 = (,,)
	<$> bitsToWord . peekOWire o1
	<*> bitsToWord . peekOWire o2
	<*> bitsToWord . peekOWire o3

tryRegisterFileWrite :: CircuitBuilder (IWire, IWire, IWire, [OWire])
tryRegisterFileWrite = do
	(cl, wr, adr, d, os) <- registerFileWrite 32
	(_, ccl) <- clockGen 5
	connectWire0 ccl cl
	return (wr, adr, d, os)

storeTryRegisterFileWrite :: (IWire, IWire, IWire, [OWire]) -> Word64 -> Word64 -> Circuit -> Circuit
storeTryRegisterFileWrite (wwr, wadr, wd, _) adr d cct = let
	cct1 = (!! 10) . iterate step $ setMultBits [wwr, wadr, wd] [0, adr, d] cct
	cct2 = (!! 20) . iterate step $ setMultBits [wwr, wadr, wd] [1, adr, d] cct1
	cct3 = (!! 10) . iterate step $ setMultBits [wwr, wadr, wd] [0, adr, d] cct2 in
	cct3

tryRegisterFile :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire, OWire)
tryRegisterFile = do
	(cl, radr1, radr2, wr, adr, d, r1, r2, _) <- registerFile 32
	(_, ccl) <- clockGen 5
	connectWire0 ccl cl
	return (radr1, radr2, wr, adr, d, r1, r2)

storeTryRegisterFile :: (IWire, IWire, IWire, IWire, IWire, OWire, OWire) -> Word64 -> Word64 -> Circuit -> Circuit
storeTryRegisterFile (_, _, wwr, wadr, wd, _, _) adr d cct = let
	cct1 = (!! 10) . iterate step $ setMultBits [wwr, wadr, wd] [0, adr, d] cct
	cct2 = (!! 20) . iterate step $ setMultBits [wwr, wadr, wd] [1, adr, d] cct1
	cct3 = (!! 10) . iterate step $ setMultBits [wwr, wadr, wd] [0, adr, d] cct2 in
	cct3

triStateSample :: CircuitBuilder (IWire, OWire)
triStateSample = do
	(sel, is, o) <- triStateSelect 8
	cs <- (constGate64 . Bits) `mapM` take 8 [0, 12 ..]
	zipWithM_ connectWire64 cs is
	return (sel, o)

tryFallingEdge :: CircuitBuilder (Clock, OWire)
tryFallingEdge = do
	cl <- clock 15
	(fi, fo) <- fallingEdge 5
	connectWire0 (clockSignal cl) fi
	return (cl, fo)

trySramWrite :: CircuitBuilder (IWire, IWire, IWire, [OWire])
trySramWrite = do
	(_, cl) <- clockGen 5
	(ei, eo) <- fallingEdge 3
	(c, w, wr) <- andGate0
	(wr', adr, d, os) <- sramWrite 8
	connectWire0 cl ei
	connectWire0 eo c
	connectWire0 wr wr'
	return (w, adr, d, os)

storeTrySramWrite :: (IWire, IWire, IWire, [OWire]) -> Word64 -> Word64 -> Circuit -> Circuit
storeTrySramWrite (ww, wadr, wd, _) adr d cct = let
	cct1 = (!! 5) . iterate step $ setMultBits [ww, wadr, wd] [0, adr, d] cct
	cct2 = (!! 25) . iterate step $ setMultBits [ww, wadr, wd] [1, adr, d] cct1
	cct3 = (!! 5) . iterate step $ setMultBits [ww, wadr, wd] [0, adr, d] cct2 in
	cct3

trySram :: Word8 -> CircuitBuilder (IWire, IWire, IWire, OWire)
trySram n = do
	(_, cl) <- clockGen 5
	(ei, eo) <- fallingEdge 3
	(c, w, wr) <- andGate0
	(wr', adr, d, o) <- sram n
	connectWire0 cl ei
	connectWire0 eo c
	connectWire0 wr wr'
	return (w, adr, d, o)

storeTrySram :: (IWire, IWire, IWire, OWire) -> Word64 -> Word64 -> Circuit -> Circuit
storeTrySram (ww, wadr, wd, _) adr d cct = let
	cct1 = (!! 7) . iterate step $ setMultBits [ww, wadr, wd] [0, adr, d] cct
	cct2 = (!! 25) . iterate step $ setMultBits [ww, wadr, wd] [1, adr, d] cct1
	cct3 = (!! 5) . iterate step $ setMultBits [ww, wadr, wd] [0, adr, d] cct2 in
	cct3
