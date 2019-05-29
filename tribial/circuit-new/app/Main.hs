{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.List
import Data.Word

import Circuit
import Element
import CircuitTools

main :: IO ()
main = putStrLn "Slozsoft"

mux2 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2 = do
	(slin, slout) <- idGate
	(ni, no) <- notGate
	(nsl, a, o1) <- andGate
	(sl, b, o2) <- andGate
	(o1', o2', c) <- orGate
	zipWithM_ connectWire [slout, no, o1, slout, o2] [ni, nsl, o1', sl, o2']
	return (slin, a, b, c)

testTri :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
testTri = do
	(oin, oout) <- idGate
	(s1, i1, o1) <- triGate
	(s2, i2, o2) <- triGate
	connectWire o1 oin
	connectWire o2 oin
	return (s1, i1, s2, i2, oout)

nor :: CircuitBuilder (IWire, IWire, OWire)
nor = do
	(i1, i2, o) <- orGate
	(o', no) <- notGate
	connectWire o o'
	return (i1, i2, no)

rs :: CircuitBuilder (IWire, IWire, OWire, OWire)
rs = do	(r, q_', q) <- nor
	(s, q', q_) <- nor
	connectWire q q'
	connectWire q_ q_'
	return (r, s, q, q_)

dlatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
dlatch = do
	(cin, cout) <- idGate
	(din, dout) <- idGate
	(d', nd) <- notGate
	(c', nd', r) <- andGate
	(c'', d'', s) <- andGate
	(r', s', q, q_) <- rs
	zipWithM_ connectWire
		[dout, cout, nd, cout, dout, r, s]
		[d', c', nd', c'', d'', r', s']
	return (cin, din, q, q_)

type Memory8Wires = (IWire, IWire, IWire, IWire, IWire, OWire)

memory8 :: CircuitBuilder Memory8Wires
memory8 = do
	(a0in, a0out) <- idGate
	(a1in, a1out) <- idGate
	(a2in, a2out) <- idGate
	(din, dout) <- idGate

	(cl, (a0, a1, a2), dec) <- (\(x, [a, b, c], d) -> (x, (a, b, c), d)) <$> dec38'

	(cs, ds, qs, _q_s) <- unzip4 <$> replicateM 8 dlatch
	zipWithM_ connectWire dec cs
	mapM_ (connectWire dout) ds

	((a0', a1', a2'), is, o) <- (\([a, b, c], d, e) -> ((a, b, c), d, e)) <$> mux8
	zipWithM_ connectWire qs is

	zipWithM_ connectWire [a0out, a1out, a2out] [a0, a1, a2]
	zipWithM_ connectWire [a0out, a1out, a2out] [a0', a1', a2']
	return (a0in, a1in, a2in, cl, din, o)

setBitsMemory8 :: Memory8Wires -> Word64 -> Bit -> Bit -> Circuit -> Circuit
setBitsMemory8 (a0, a1, a2, c, d, _) a bc bd =
	foldr (.) id (zipWith setBit [a0, a1, a2] $ wordToBits 64 a)
		. setBit c bc . setBit d bd

getBitsMemory8 :: Memory8Wires -> Circuit -> Bit
getBitsMemory8 (_, _, _, _, _, o) = peekOWire o

setAndRunMemory8 ::
	Memory8Wires -> Word64 -> Bit -> Bit -> Int -> Circuit -> (Bit, Circuit)
setAndRunMemory8 ws a bc bd i cct = let
	cct' = (!! i) . iterate step $ setBitsMemory8 ws a bc bd cct in
	(getBitsMemory8 ws cct', cct')

type Mux8Wires = ([IWire], [IWire], OWire)
	
mux8 :: CircuitBuilder Mux8Wires
mux8 = do
	(ms, ss) <- dec38
	(ss', is, os) <- unzip3 <$> replicateM 8 triGate
	(oin, oout) <- idGate
	zipWithM_ connectWire ss ss'
	mapM_ (`connectWire` oin) os
	return (ms, is, oout)

setBitsMux8 :: Mux8Wires -> Word64 -> [Bit] -> Circuit -> Circuit
setBitsMux8 (ms, is, _) m bs = foldr (.) id (zipWith setBit ms $ wordToBits 64 m)
	. foldr (.) id (zipWith setBit is bs)

getBitsMux8 :: Mux8Wires -> Circuit -> Bit
getBitsMux8 (_, _, o) = peekOWire o

dec38' :: CircuitBuilder (IWire, [IWire], [OWire])
dec38' = do
	(cin, cout) <- idGate
	(ins, outs) <- dec38
	(ai1s, ai2s, aos) <- unzip3 <$> replicateM 8 andGate
	mapM_ (connectWire cout) ai1s
	zipWithM_ connectWire outs ai2s
	return (cin, ins, aos)

dec38 :: CircuitBuilder ([IWire], [OWire])
dec38 = do
	(is, ois) <- unzip <$> replicateM 3 idGate
	(ias, oas) <- unzip <$> replicateM 8 andGate3
	zipWithM_ ((sequence_ .) . flip (zipWith3 id) ois) (binary (inverse, obverse) 3) ias
	return (is, oas)

andGate3 :: CircuitBuilder ([IWire], OWire)
andGate3 = do
	(i1, i2, a1) <- andGate
	(a1', i3, o) <- andGate
	connectWire a1 a1'
	return ([i1, i2, i3], o)

delayedNot :: CircuitBuilder (IWire, OWire)
delayedNot = do
	(ni, no) <- notGate
	(dli, dlo) <- delay 5
	connectWire no dli
	return (ni, dlo)

innerNot :: (Circuit, IWire, OWire)
innerNot = let ((i, o), cct) = makeCircuit delayedNot in (cct, i, o)

app2of3 :: (b -> d) -> (a, b, c) -> (a, d, c)
app2of3 f (x, y, z) = (x, f y, z)

innerRs :: CircuitBuilder ([IWire], OWire)
innerRs = do
	(r, s, q, _q_) <- rs
	return ([r, s], q)

type TestLazyGate2Wires = ([IWire], IWire, IWire, OWire)

testLazyGate2 :: CircuitBuilder TestLazyGate2Wires
testLazyGate2 = do
	(ix, is, o) <- lazyGates 3 innerRs
	return (ix, is !! 0, is !! 1, o)

setBitsTestLazyGate2 ::
	TestLazyGate2Wires -> Word64 -> Bit -> Bit -> Circuit -> Circuit
setBitsTestLazyGate2 (wix, wr, ws, _) ix br bs =
	foldr (.) id (zipWith setBit wix $ wordToBits 64 ix)
		. setBit wr br . setBit ws bs

setAndRunTestLazyGate2 ::
	TestLazyGate2Wires -> Word64 -> Bit -> Bit -> Int -> Circuit -> Circuit
setAndRunTestLazyGate2 ws ix br bs n = run n . setBitsTestLazyGate2 ws ix br bs 

getBitsTestLazyGate2 :: TestLazyGate2Wires -> Circuit -> Bit
getBitsTestLazyGate2 (_, _, _, o) = peekOWire o

dlatchInner :: CircuitBuilder ([IWire], OWire)
dlatchInner = do
	(cin, cout) <- idGate
	(din, dout) <- idGate
	(ni, no) <- notGate
	(ar1, ar2, aro) <- andGate
	(as1, as2, aso) <- andGate
	(rs', q) <- innerRs
	let	r = rs' !! 0
		s = rs' !! 1
	connectWire dout ni
	connectWire cout ar1
	connectWire no ar2
	connectWire cout as1
	connectWire dout as2
	connectWire aro r
	connectWire aso s
	return ([cin, din], q)

testLazyDlatch :: CircuitBuilder ([IWire], IWire, IWire, OWire)
testLazyDlatch = do
	(ix, is, o) <- lazyGates 3 dlatchInner
	return (ix, is !! 0, is !! 1, o)
