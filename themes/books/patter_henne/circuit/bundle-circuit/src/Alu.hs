{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Alu (
	RiscvAlu(..), riscvAlu, peekRiscvAlu, setRiscvAlu,
	RiscvAdder(..), riscvAdder, peekRiscvAdder, setRiscvAdder,
	RiscvSubtractor(..), riscvSubtractor,
	RiscvAluWires, adder64', sum64
	) where

import Control.Monad
import Data.Word

import Circuit
import CarryLookahead
import Element
import Tools

data RiscvAlu = RiscvAlu {
	aluOpcode :: IWire, aluArgA :: IWire, aluArgB :: IWire,
	aluResult :: OWire, aluZero :: OWire, aluOverflow :: OWire
	} deriving Show

newtype Opcode = Opcode { opcode :: Word64 } deriving Show

riscvAlu :: CircuitBuilder RiscvAlu
riscvAlu = do
	(op, a, b, r, z, ovfl) <- riscvAluGen
	return $ RiscvAlu op a b r z ovfl

{-
opAnd, opOr, opAdd, opSub, opSlt, opNor :: Opcode
[opAnd, opOr, opAdd, opSub, opSlt, opNor] =
	Opcode <$> [0b0000, 0b0001, 0b0010, 0b0110, 0b0111, 0b1100]
	-}

setRiscvAlu :: RiscvAlu -> Opcode -> Word64 -> Word64 -> Circuit -> Circuit
setRiscvAlu rva op a b = setBits (aluOpcode rva) (wordToBits $ opcode op)
	. setBits (aluArgA rva) (wordToBits a)
	. setBits (aluArgB rva) (wordToBits b)

peekRiscvAlu :: RiscvAlu -> Circuit -> (Word64, Word64, Word64)
peekRiscvAlu rva cct = listToTuple3
	$ peekMultOWires (($ rva) <$> [aluResult, aluZero, aluOverflow]) cct

data RiscvAdder = RiscvAdder {
	addrArgA :: IWire, addrArgB :: IWire,
	addrResult :: OWire, addrOverflow :: OWire }
	deriving Show

riscvAdder :: CircuitBuilder RiscvAdder
riscvAdder = do
	rva <- riscvAlu
	opad <- constGate (Bits 0b0010) 4 0 0
	connectWire (opad, 4, 0) (aluOpcode rva, 4, 0)
	return RiscvAdder {
		addrArgA = aluArgA rva, addrArgB = aluArgB rva,
		addrResult = aluResult rva, addrOverflow = aluOverflow rva }

setRiscvAdder :: RiscvAdder -> Word64 -> Word64 -> Circuit -> Circuit
setRiscvAdder rvad a b = setBits (addrArgA rvad) (wordToBits a)
	. setBits (addrArgB rvad) (wordToBits b)

peekRiscvAdder :: RiscvAdder -> Circuit -> (Word64, Word64)
peekRiscvAdder rvad = peekOWire2 (addrResult rvad) (addrOverflow rvad)

data RiscvSubtractor = RiscvSubtractor {
	sbrArgA :: IWire, sbrArgB :: IWire,
	sbrResult :: OWire, sbrZero :: OWire, sbrOverflow :: OWire }
	deriving Show

riscvSubtractor :: CircuitBuilder RiscvSubtractor
riscvSubtractor = do
	rva <- riscvAlu
	opsb <- constGate (Bits 0b0110) 4 0 0
	connectWire (opsb, 4, 0) (aluOpcode rva, 4, 0)
	return RiscvSubtractor {
		sbrArgA = aluArgA rva, sbrArgB = aluArgB rva,
		sbrResult = aluResult rva, sbrZero = aluZero rva,
		sbrOverflow = aluOverflow rva }

type RiscvAluWires = (IWire, IWire, IWire, OWire, OWire, OWire)

riscvAluGen :: CircuitBuilder RiscvAluWires
riscvAluGen = do
	(opin, opout) <- idGate 4 0 0
	(ainv, binv, op, ci, a, b, r, z, ovfl) <- alu
	connectWire (opout, 2, 0) (op, 2, 0)
	connectWire (opout, 1, 2) (binv, 1, 0)
	connectWire (opout, 1, 2) (ci, 1, 0)
	connectWire (opout, 1, 3) (ainv, 1, 0)
	return (opin, a, b, r, z, ovfl)

alu :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, IWire, OWire, OWire, OWire)
alu = do
	(ainv, ain, aout) <- invert
	(binv, bin, bout) <- invert
	(aa, ab, ao) <- andGate64
	(oa, ob, oo) <- orGate64
	(ci, sa, sb, ss, slt, ovfl) <- adder64'

	(lessin, lessout) <- idGate64
	zbs <- constGate (Bits 0) 63 1 1
	connectWire (zbs, 63, 1) (lessin, 63, 1)
	connectWire (slt, 1, 0) (lessin, 1, 0)

	(op, ms, r) <- multiplexer 4
	let	(m0, m1, m2, m3) = listToTuple4 ms
	zipWithM_ connectWire64
		[aout, bout, aout, bout, aout, bout, ao, oo, ss, lessout]
		[aa, ab, oa, ob, sa, sb, m0, m1, m2, m3]

	(r', nzero) <- orGateAllBits64
	(nzero', zero) <- notGate 1 0 0
	connectWire64 r r'
	connectWire0 nzero nzero'

	return (ainv, binv, op, ci, ain, bin, r, zero, ovfl)

adder64' :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire, OWire)
adder64' = do
	(ciin, ciout) <- idGate0
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(cs, co64) <- carries ciout aout bout
	(ci, a, b, s) <- sum64
	zipWithM_ connectWire64 [cs, aout, bout] [ci, a, b]

	(ofci, ofco, ovfl) <- overflow
	(ltsm, ltof, slt) <- lessthan
	connectWire (cs, 1, 63) (ofci, 1, 63)
	connectWire (co64, 1, 0) (ofco, 1, 0)
	connectWire (s, 1, 63) (ltsm, 1, 0)
	connectWire (ovfl, 1, 0) (ltof, 1, 0)

	return (ciin, ain, bin, s, slt, ovfl)

overflow :: CircuitBuilder (IWire, IWire, OWire)
overflow = xorGate 1 63 0 0

lessthan :: CircuitBuilder (IWire, IWire, OWire)
lessthan = xorGate 1 63 0 0

sum64 :: CircuitBuilder (IWire, IWire, IWire, OWire)
sum64 = do
	((ci, a, b), s) <- xorGate3 64 0
	return (ci, a, b, s)
