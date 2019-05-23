{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscvAlu where

import Control.Monad
import Data.List
import Data.Word

import FastAdderBinary
import Circuit
import Element
import Tools

alu1_0 :: CircuitBuilder Wires31
alu1_0 = do
	((ain, aout), (bin, bout)) <- listToTuple2 <$> replicateM 2 idGate
	(a', b', ado) <- andGate
	(a'', b'', oro) <- orGate
	(op, adi, ori, r) <- mux2
	zipWithM_ connectWire
		[aout, bout, ado, aout, bout, oro]
		[a', b', adi, a'', b'', ori]
	return (op, ain, bin, r)

sum1 :: CircuitBuilder Wires31
sum1 = do
	((ciin, ciout, rciout), (ain, aout, raout), (bin, bout, rbout)) <-
		listToTuple3 <$> replicateM 3 obrev
	(((ci', ra', rb'), o1o), ((rci'', a'', rb''), o2o),
			((rci''', ra''', b'''), o3o),
			((ci'''', a'''', b''''), o4o)) <-
		listToTuple4 <$> replicateM 4 andGate3
	((o1, o2, o3, o4), s) <- orGate4
	zipWithM_ connectWire
		[ciout, raout, rbout, rciout, aout, rbout, rciout, raout, bout]
		[ci', ra', rb', rci'', a'', rb'', rci''', ra''', b''']
	zipWithM_ connectWire
		[ciout, aout, bout, o1o, o2o, o3o, o4o]
		[ci'''', a'''', b'''', o1, o2, o3, o4]
	return (ciin, ain, bin, s)

alu1_1 :: CircuitBuilder Wires51
alu1_1 = do
	((ain, aout), (bin, bout)) <- listToTuple2 <$> replicateM 2 idGate
	(aad, bad, ad) <- andGate
	(aor, bor, o) <- orGate
	(ci, as, bs, s) <- sum1
	(o0, o1, adm, om, sm, r) <- mux3
	zipWithM_ connectWire
		[aout, bout, ad, aout, bout, o, aout, bout, s]
		[aad, bad, adm, aor, bor, om, as, bs, sm]
	return (o0, o1, ci, ain, bin, r)

type Alu_1Wires = (IWire, IWire, IWire, [IWire], [IWire], [OWire], OWire)

alu_1 :: Word8 -> CircuitBuilder Alu_1Wires
alu_1 n = do
	(o0in, o0out) <- idGate
	(o1in, o1out) <- idGate
	(c0in, c0out) <- idGate
	(ains, aouts) <- unzip <$> replicateM (2 ^ n) idGate
	(bins, bouts) <- unzip <$> replicateM (2 ^ n) idGate
	(c0', as', bs', co1_2n, _g, _p) <- carry1_2n n
	(o0s, o1s, ci0_2n_1, as'', bs'', rs) <- unzip6 <$> replicateM (2 ^ n) alu1_1
	connectWire c0out c0'
	zipWithM_ connectWire aouts as'
	zipWithM_ connectWire bouts bs'
	zipWithM_ (\o0 o1 -> connectWire o0out o0 >> connectWire o1out o1) o0s o1s
	zipWithM_ connectWire (c0out : co1_2n) ci0_2n_1
	zipWithM_ connectWire aouts as''
	zipWithM_ connectWire bouts bs''
	return (o0in, o1in, c0in, ains, bins, rs, last co1_2n)

setBitsAlu_1 :: Alu_1Wires ->
	Bit -> Bit -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setBitsAlu_1 (o0, o1, c0, as, bs, _, _) bo0 bo1 bc0 a b =
	foldr (.) id (zipWith setBit [o0, o1, c0] [bo0, bo1, bc0])
		. foldr (.) id (zipWith setBit as $ wordToBits 64 a)
		. foldr (.) id (zipWith setBit bs $ wordToBits 64 b)

peekBitsAlu_1 :: Alu_1Wires -> Circuit -> (Word64, Bit)
peekBitsAlu_1 (_, _, _, _, _, rs, co) cct =
	(bitsToWord $ (`peekOWire` cct) <$> rs, peekOWire co cct)
