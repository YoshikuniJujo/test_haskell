{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Alu where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Word

import Circuit
import Element
import CircuitTools
import CarryLookahead

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

alu1_ao :: CircuitBuilder Wires31
alu1_ao = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(s, a, o, r) <- mux2
	zipWithM_ connectWire
		[aout, bout, aout, bout, ao, oo]
		[aa, ab, oa, ob, a, o]
	return (s, ain, bin, r)

carry1 :: CircuitBuilder Wires31
carry1 = do
	(ciin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(ci1, a1, ad1) <- andGate
	(a2, b2, ad2) <- andGate
	(b3, ci3, ad3) <- andGate
	((o1, o2, o3), oo) <- orGate3
	zipWithM_ connectWire
		[ciout, aout, aout, bout, bout, ciout, ad1, ad2, ad3]
		[ci1, a1, a2, b2, b3, ci3, o1, o2, o3]
	return (ciin, ain, bin, oo)

sum1 :: CircuitBuilder Wires31
sum1 = do
	(a, b, abo) <- xorGate
	(abi, ci, r) <- xorGate
	connectWire abo abi
	return (ci, a, b, r)

adder1 :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
adder1 = do
	(ciin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(c1, a1, b1, s) <- sum1
	(c2, a2, b2, co) <- carry1
	zipWithM_ connectWire
		[ciout, aout, bout, ciout, aout, bout] [c1, a1, b1, c2, a2, b2]
	return (ciin, ain, bin, s, co)

alu1_aos :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, OWire, OWire)
alu1_aos = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ci, sa, sb, sm, co) <- adder1
	(op, mx0, mx1, mx2, mo) <- mux3_1
	zipWithM_ connectWire
		[aout, bout, aout, bout, aout, bout, ao, oo, sm]
		[aa, ab, oa, ob, sa, sb, mx0, mx1, mx2]
	return (op, ci, ain, bin, mo, co)

type AluAosWires = ((IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire)

alu_aos :: Word8 -> CircuitBuilder AluAosWires
alu_aos n | n < 1 || n > 64 = error "Oops!"
alu_aos 1 = do
	(op, ci, a, b, r, co) <- alu1_aos
	return (op, ci, [a], [b], [r], co)
alu_aos n = do
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	((op01, op11), ci0, a1, b1, r1, co1) <- alu1_aos
	((op02, op12), ci1, as, bs, rs, con) <- alu_aos (n - 1)
	(connectWire op0out) `mapM_` [op01, op02]
	(connectWire op1out) `mapM_` [op11, op12]
	connectWire co1 ci1
	return ((op0in, op1in), ci0, a1 : as, b1 : bs, r1 : rs, con)

setBitsAluAos :: AluAosWires -> Word64 -> Bit -> Word64 -> Word64 -> DoCircuit
setBitsAluAos ((wop0, wop1), wci, was, wbs, _, _) op bci a b =
	setBits [wop0, wop1] (wordToBits 64 op) . setBit wci bci
		. setBits was (wordToBits 64 a) . setBits wbs (wordToBits 64 b)

getBitsAluAosBits :: AluAosWires -> Circuit -> ([Bit], Bit)
getBitsAluAosBits (_, _, _, _, wrs, wco) cct =
	((`peekOWire` cct) <$> wrs, peekOWire wco cct)

setAndRunAluAos ::
	AluAosWires -> Word64 -> Bit -> Word64 -> Word64 -> Int -> DoCircuit
setAndRunAluAos ws op bci a b n = run n . setBitsAluAos ws op bci a b

getBitsAluAos :: AluAosWires -> Circuit -> (Word64, Bit)
getBitsAluAos ws = first bitsToWord . getBitsAluAosBits ws

alu1_aos' :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, OWire)
alu1_aos' = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ci, sa, sb, sm) <- sum1
	(op, mx0, mx1, mx2, mo) <- mux3_1
	zipWithM_ connectWire
		[aout, bout, aout, bout, aout, bout, ao, oo, sm]
		[aa, ab, oa, ob, sa, sb, mx0, mx1, mx2]
	return (op, ci, ain, bin, mo)

alu_aos' :: Word8 -> CircuitBuilder AluAosWires
alu_aos' n = do
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	(ci0in, ci0out) <- idGate
	(ains, aouts) <- unzip <$> fromIntegral n `replicateM` idGate
	(bins, bouts) <- unzip <$> fromIntegral n `replicateM` idGate
	(ci0, as, bs, cots, _, _) <- carries n
	(ops, cins, as', bs', rs) <- unzip5 <$> fromIntegral n `replicateM` alu1_aos'
	let	(op0s, op1s) = unzip ops
	connectWire op0out `mapM_` op0s
	connectWire op1out `mapM_` op1s
	connectWire ci0out ci0
	zipWithM_ connectWire aouts as
	zipWithM_ connectWire bouts bs
	zipWithM_ connectWire (ci0out : cots) cins
	zipWithM_ connectWire aouts as'
	zipWithM_ connectWire bouts bs'
	return ((op0in, op1in), ci0in, ains, bins, rs, last cots)

alu1_aosd :: CircuitBuilder (IWire, (IWire, IWire), IWire, IWire, IWire, OWire)
alu1_aosd = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(nbi, nbo) <- notGate
	(binv, b, nb, b') <- mux2
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ci, sa, sb, sm) <- sum1
	(op, mx0, mx1, mx2, mo) <- mux3_1
	zipWithM_ connectWire [bout, bout, nbo] [nbi, b, nb]
	zipWithM_ connectWire
		[aout, b', aout, b', aout, b', ao, oo, sm]
		[aa, ab, oa, ob, sa, sb, mx0, mx1, mx2]
	return (binv, op, ci, ain, bin, mo)

type AluAosdnWires = (IWire, IWire, IWirePair, IWire, [IWire], [IWire], [OWire], OWire)

complementAll :: [IWire] -> CircuitBuilder (IWire, [IWire])
complementAll is = do
	(invin, invout) <- idGate
	(dins, douts) <- unzip <$> n `replicateM` idGate
	(nis, nos) <- unzip <$> n `replicateM` notGate
	(invs, ds, nds, os) <- unzip4 <$> n `replicateM` mux2
	connectWire invout `mapM_` invs
	zipWithM_ connectWire douts nis
	zipWithM_ connectWire douts ds
	zipWithM_ connectWire nos nds
	zipWithM_ connectWire os is
	return (invin, dins)
	where n = length is

alu_aosdn :: Word8 -> CircuitBuilder AluAosdnWires
alu_aosdn n = do
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	(ci0in, ci0out) <- idGate
	(ains, aouts) <- unzip <$> fromIntegral n `replicateM` idGate
	(bins, bouts) <- unzip <$> fromIntegral n `replicateM` idGate
	(ci0, as, bs, cots, _, _) <- carries n
	(ops, cins, as', bs', rs) <-
		unzip5 <$> fromIntegral n `replicateM` alu1_aos'
	let	(op0s, op1s) = unzip ops
	connectWire op0out `mapM_` op0s
	connectWire op1out `mapM_` op1s
	connectWire ci0out ci0
	zipWithM_ connectWire aouts as
	zipWithM_ connectWire bouts bs
	zipWithM_ connectWire (ci0out : cots) cins
	zipWithM_ connectWire aouts as'
	zipWithM_ connectWire bouts bs'
	(ainv, ains') <- complementAll ains
	(binv, bins') <- complementAll bins
	return (ainv, binv, (op0in, op1in), ci0in, ains', bins', rs, last cots)

setBitsAluAosdn :: AluAosdnWires ->
	Bit -> Bit -> Word64 -> Bit -> Word64 -> Word64 -> DoCircuit
setBitsAluAosdn (wainv, wbinv, (wop0, wop1), wci0, was, wbs, _, _)
		bainv bbinv op bci0 a b =
	setBit wainv bainv . setBit wbinv bbinv
		. setBits [wop0, wop1] (wordToBits 64 op)
		. setBit wci0 bci0
		. setBits was (wordToBits 64 a) . setBits wbs (wordToBits 64 b)

getBitsAluAosdnBits :: AluAosdnWires -> Circuit -> ([Bit], Bit)
getBitsAluAosdnBits (_, _, _, _, _, _, rs, co) cct =
	((`peekOWire` cct) <$> rs, peekOWire co cct)

getBitsAluAosdn :: AluAosdnWires -> Circuit -> (Word64, Bit)
getBitsAluAosdn ws = first bitsToWord . getBitsAluAosdnBits ws

setAndRunAluAosdn :: AluAosdnWires ->
	Bit -> Bit -> Word64 -> Bit -> Word64 -> Word64 -> Int -> DoCircuit
setAndRunAluAosdn ws bainv bbinv op bci0 a b n =
	run n . setBitsAluAosdn ws bainv bbinv op bci0 a b
