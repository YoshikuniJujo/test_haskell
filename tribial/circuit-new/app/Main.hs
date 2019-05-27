{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad

import Circuit

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
