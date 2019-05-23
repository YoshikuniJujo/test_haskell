{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscvAlu where

import Control.Monad

import Circuit
import Element
import Tools

alu1_0 :: CircuitBuilder (IWire, IWire, IWire, OWire)
alu1_0 = do
	((ain, aout), (bin, bout)) <- listToTuple2 <$> replicateM 2 idGate
	(a', b', ado) <- andGate
	(a'', b'', oro) <- orGate
	(op, adi, ori, r) <- mux2
	zipWithM_ connectWire
		[aout, bout, ado, aout, bout, oro]
		[a', b', adi, a'', b'', ori]
	return (op, ain, bin, r)

sum1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
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
