{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.Alu (riscvAlu, riscvAdder) where

import Control.Monad
import Data.List
import Data.Word

import Circuit
import Element
import CarryLookahead

import RiscV.Element

sum1 :: CircuitBuilder Wires31
sum1 = do
	(a, b, abo) <- xorGate
	(abi, ci, r) <- xorGate
	connectWire abo abi
	return (ci, a, b, r)

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

alu1_aosl :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, IWire, OWire)
alu1_aosl = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ci, sa, sb, sm) <- sum1
	(op, mx0, mx1, mx2, less, mo) <- mux4_1
	zipWithM_ connectWire
		[aout, bout, aout, bout, aout, bout, ao, oo, sm]
		[aa, ab, oa, ob, sa, sb, mx0, mx1, mx2]
	return (op, ci, ain, bin, less, mo)

alu1_msb :: CircuitBuilder ((IWire, IWire), IWire, IWire, IWire, IWire, IWire, OWire, OWire, OWire)
alu1_msb = do
	(ciin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ci, sa, sb, sm) <- sum1
	(op, mx0, mx1, mx2, less, mo) <- mux4_1
	zipWithM_ connectWire
		[ciout, aout, bout, aout, bout, aout, bout, ao, oo, sm]
		[ci, aa, ab, oa, ob, sa, sb, mx0, mx1, mx2]
	(ofci, co, ovfl) <- overflow
	(sltsn, sltof, slt) <- lessthan
	zipWithM_ connectWire [ciout, sm, ovfl] [ofci, sltsn, sltof]
	return (op, ciin, ain, bin, less, co, mo, slt, ovfl)

type AluWires =
	(IWire, IWire, (IWire, IWire), IWire, [IWire], [IWire], [OWire], OWire)

alu :: Word8 -> CircuitBuilder AluWires
alu n = do
	(op0in, op0out) <- idGate
	(op1in, op1out) <- idGate
	(ci0in, ci0out) <- idGate
	(ains, aouts) <- unzip <$> fromIntegral n `replicateM` idGate
	(bins, bouts) <- unzip <$> fromIntegral n `replicateM` idGate
	(ci0, as, bs, cots, _, _) <- carries n
	(ops, cins, as', bs', less, rs) <-
		unzip6 <$> fromIntegral (n - 1) `replicateM` alu1_aosl
	((op0msb, op1msb), cimsb, amsb, bmsb, lessmsb, comsb, rmsb, slt, ovfl)
		<- alu1_msb
	let	(op0s, op1s) = unzip ops
	connectWire op0out `mapM_` (op0s ++ [op0msb])
	connectWire op1out `mapM_` (op1s ++ [op1msb])
	connectWire ci0out ci0
	zipWithM_ connectWire aouts as
	zipWithM_ connectWire bouts bs
	zipWithM_ connectWire (ci0out : cots) $ cins ++ [cimsb]
	zipWithM_ connectWire aouts $ as' ++ [amsb]
	zipWithM_ connectWire bouts $ bs' ++ [bmsb]
	(ainv, ains') <- complementAll ains
	(binv, bins') <- complementAll bins

	zero <- constGate O
	connectWire zero `mapM_` (tail less ++ [lessmsb])
	connectWire slt $ head less
	connectWire (last cots) comsb
	return (ainv, binv,
		(op0in, op1in), ci0in, ains', bins', rs ++ [rmsb], ovfl)

-- overflow = carry in `xor` carry out
-- less than = sum (sign) `xor` overflow

overflow :: CircuitBuilder (IWire, IWire, OWire)
overflow = xorGate

lessthan :: CircuitBuilder (IWire, IWire, OWire)
lessthan = xorGate

type RiscvAluWires = ((IWire, IWire, IWire, IWire), [IWire], [IWire], [OWire], OWire, OWire)
-- ((op0, op1, bneg, ainv), as, bs, rs, zero, ovfl)

riscvAlu :: CircuitBuilder RiscvAluWires
riscvAlu = do
	(bnegin, bnegout) <- idGate
	(ainv, binv, (op0, op1), ci0, as, bs, rs, ovfl) <- alu 64
	(zins, zout) <- multiOrGate 64
	(ni, zero) <- notGate
	connectWire bnegout `mapM_` [binv, ci0]
	zipWithM_ connectWire rs zins
	connectWire zout ni
	return ((op0, op1, bnegin, ainv), as, bs, rs, zero, ovfl)

riscvAdder :: CircuitBuilder ([IWire], [IWire], [OWire])
riscvAdder = do
	zero <- constGate O
	one <- constGate I
	((op0, op1, bneg, ainv), as, bs, rs, _zero, _ovfl) <- riscvAlu
	connectWire zero `mapM_` [op0, bneg, ainv]
	connectWire one op1
	return (as, bs, rs)
