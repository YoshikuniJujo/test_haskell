{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryCircuit where

import Control.Monad
import Data.Word

import Circuit
import Element
import CarryLookahead
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

sum64 :: CircuitBuilder (IWire, IWire, IWire, OWire)
sum64 = do
	((ci, a, b), s) <- xorGate3 64 0
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

adder64' :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
adder64' = do
	(ciin, ciout) <- idGate0
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(cs, co64) <- carries ciout aout bout
	(ci, a, b, s) <- sum64
	zipWithM_ connectWire64 [cs, aout, bout] [ci, a, b]
	return (ciin, ain, bin, s, co64)

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
	(ci, sa, sb, ss, co) <- adder64'
	(op, ms, r) <- multiplexer 3
	let	(m0, m1, m2) = listToTuple3 ms
	zipWithM_ connectWire64
		[aout, bout', aout, bout', aout, bout', ao, oo, ss]
		[aa, ab, oa, ob, sa, sb, m0, m1, m2]
	return (binv, op, ci, ain, bin, r, co)

mkSampleAluAos64 :: CircuitBuilder
	(IWire, IWire, IWire, IWire, IWire, OWire, OWire) -> [Word64]
mkSampleAluAos64 u = let
	((binv, op, ci, a, b, r, _co), cct) = makeCircuit u
	cct0 = setBits binv (Bits 1) . setBits op (Bits 2) . setBits ci (Bits 1)
		. setBits a (Bits 987654321) $ setBits b (Bits 123456789) cct in
	bitsToWord . peekOWire r <$> iterate step cct0
