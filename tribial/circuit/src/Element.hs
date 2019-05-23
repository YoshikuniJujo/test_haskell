{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Control.Monad

import Circuit
import Tools

mux2 :: CircuitBuilder Wires31
mux2 = do
	(si, sout) <- idGate
	(s', nsout) <- notGate
	(a, ns', o1out) <- andGate
	(b, s'', o2out) <- andGate
	(o1, o2, r) <- orGate
	zipWithM_ connectWire
		[sout, nsout, o1out, sout, o2out]
		[s', ns', o1, s'', o2]
	return (si, a, b, r)

mux3 :: CircuitBuilder Wires51
mux3 = do
	(o0, o1, s0, s1, s2) <- dec23
	((a, s0', ao), (b, s1', bo), (c, s2', co)) <- 
		listToTuple3 <$> replicateM 3 andGate
	((ao', bo', co'), r) <- orGate3
	zipWithM_ connectWire
		[s0, ao, s1, bo, s2, co]
		[s0', ao', s1', bo', s2', co']
	return (o0, o1, a, b, c, r)

dec23 :: CircuitBuilder (IWire, IWire, OWire, OWire, OWire)
dec23 = do
	((b0in, b0out, rb0out), (b1in, b1out, rb1out)) <-
		listToTuple2 <$> replicateM 2 obrev
	((r01, r02, r0), (r11, r12, r1), (r21, r22, r2)) <-
		listToTuple3 <$> replicateM 3 andGate
	zipWithM_ connectWire
		[rb0out, rb1out, b0out, rb1out, rb0out, b1out]
		[r01, r02, r11, r12, r21, r22]
	return (b0in, b1in, r0, r1, r2)

orGate5, andGate5 :: CircuitBuilder ((IWire, IWire, IWire, IWire, IWire), OWire)
orGate5 = first listToTuple5 <$> multiOrGate 5
andGate5 = first listToTuple5 <$> multiAndGate 5

orGate4, andGate4 :: CircuitBuilder ((IWire, IWire, IWire, IWire), OWire)
orGate4 = first listToTuple4 <$> multiOrGate 4
andGate4 = first listToTuple4 <$> multiAndGate 4

orGate3, andGate3 :: CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate3 = first listToTuple3 <$> multiOrGate 3
andGate3 = first listToTuple3 <$> multiAndGate 3

multiOrGate, multiAndGate :: Int -> CircuitBuilder ([IWire], OWire)
multiOrGate = multiple orGate
multiAndGate = multiple andGate

multiple :: CircuitBuilder (IWire, IWire, OWire) ->
	Int -> CircuitBuilder ([IWire], OWire)
multiple _ n | n < 1 = error "Oops!"
multiple _ 1 = first (: []) <$> idGate
multiple g 2 = (\(i1, i2, o) -> ([i1, i2], o)) <$> g
multiple g n = do
	(is1, o1) <- multiple g (n `div` 2)
	(is2, o2) <- multiple g (n - n `div` 2)
	(i1, i2, o) <- g
	connectWire o1 i1
	connectWire o2 i2
	return (is1 ++ is2, o)

obrev :: CircuitBuilder (IWire, OWire, OWire)
obrev = do
	(i, ob) <- idGate
	(ob', rev) <- notGate
	connectWire ob ob'
	return (i, ob, rev)

flipIf :: IWire -> CircuitBuilder (IWire, IWire)
flipIf x = do
	(i, o) <- idGate
	(ni, no) <- notGate
	(si, a, b, mo) <- mux2
	connectWire o ni
	connectWire o a
	connectWire no b
	connectWire mo x
	return (si, i)
