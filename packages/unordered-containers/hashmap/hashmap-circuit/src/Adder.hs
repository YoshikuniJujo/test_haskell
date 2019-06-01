{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Adder where

import Prelude hiding (sum)

import Control.Monad
import Data.Word

import Circuit
import CircuitTools
import Element

import MapCircuit
import HashMapCircuit

sum :: CircuitBuilderConstraint g c => CircuitBuilder g c (IWire, IWire, IWire, OWire)
sum = do
	((c, a, b), s) <- xorGate3
	return (c, a, b, s)

carry :: CircuitBuilderConstraint g c => CircuitBuilder g c (IWire, IWire, IWire, OWire)
carry = do
	(cin, cout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(a1, b1, i1) <- andGate
	(c2, b2, i2) <- andGate
	(c3, a3, i3) <- andGate
	((o1, o2, o3), co) <- orGate3
	zipWithM_ connectWire
		[aout, bout, cout, bout, cout, aout]
		[a1, b1, c2, b2, c3, a3]
	zipWithM_ connectWire [i1, i2, i3] [o1, o2, o3]
	return (cin, ain, bin, co)

adder :: CircuitBuilderConstraint g c =>
	CircuitBuilder g c (IWire, IWire, IWire, OWire, OWire)
adder = do
	(cin, cout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(sc, sa, sb, s) <- sum
	(cc, ca, cb, c) <- carry
	zipWithM_ connectWire [cout, aout, bout] [sc, sa, sb]
	zipWithM_ connectWire [cout, aout, bout] [cc, ca, cb]
	return (cin, ain, bin, s, c)

adderN :: CircuitBuilderConstraint g c =>
	Word8 -> CircuitBuilder g c (IWire, [IWire], [IWire], [OWire], OWire)
adderN 0 = error "adderN: n is not 0"
adderN 1 = do
	(ci, a, b, s, co) <- adder
	return (ci, [a], [b], [s], co)
adderN n = do
	(ci0, a0, b0, s0, co1) <- adder
	(ci1, as, bs, ss, con_1) <- adderN (n - 1)
	connectWire co1 ci1
	return (ci0, a0 : as, b0 : bs, s0 : ss, con_1)

adder64 :: CircuitBuilderConstraint g c =>
	CircuitBuilder g c (IWire, [IWire], [IWire], [OWire], OWire)
adder64 = adderN 64

subtractN :: CircuitBuilderConstraint g c =>
	Word8 -> CircuitBuilder g c ([IWire], [IWire], [OWire], OWire)
subtractN n = do
	ci <- constGate I
	(bs, nbs) <- unzip <$> fromIntegral n `replicateM` notGate
	(ci', as, nbs', ss, co) <- adderN n
	connectWire ci ci'
	zipWithM_ connectWire nbs nbs'
	return (as, bs, ss, co)

subtract64 :: CircuitBuilderConstraint g c =>
	CircuitBuilder g c ([IWire], [IWire], [OWire], OWire)
subtract64 = subtractN 64

subtractExample :: CircuitConstraint g c s => ([OWire], Circuit g c s)
subtractExample = let
	((as, bs, ss, _co), cct) = makeCircuit subtract64 in
	(ss, foldr (.) id (zipWith setBit as (numToBits 64 (123 :: Word64)))
		$ foldr (.) id (zipWith setBit bs (numToBits 64 (32 :: Word64))) cct)

subtractExampleMap :: Word64
subtractExampleMap = let (ss, cct) = subtractExample :: ([OWire], MapCircuit) in
	(!! 700) $ bitsToNum . (\c -> (`peekOWire` c) <$> ss) <$> iterate step cct

subtractExampleHashMap :: Word64
subtractExampleHashMap = let (ss, cct) = subtractExample :: ([OWire], HashMapCircuit) in
	(!! 700) $ bitsToNum . (\c -> (`peekOWire` c) <$> ss) <$> iterate step cct
