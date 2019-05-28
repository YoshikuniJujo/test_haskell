{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Memory where

import Control.Monad
import Data.List
import Data.Word

import Circuit
import Element
import CircuitTools

rslatch :: CircuitBuilder Wires22
rslatch = do
	(r, q_', q) <- norGate
	(s, q', q_) <- norGate
	zipWithM_ connectWire [q, q_] [q', q_']
	return (r, s, q, q_)

dlatch :: CircuitBuilder Wires22
dlatch = do
	(cin, cout) <- idGate
	(din, dout) <- idGate
	(ni, no) <- notGate
	(ra1, ra2, r) <- andGate
	(sa1, sa2, s) <- andGate
	(r', s', q, q_) <- rslatch
	zipWithM_ connectWire
		[dout, cout, no, cout, dout, r, s]
		[ni, ra1, ra2, sa1, sa2, r', s']
	return (cin, din, q, q_)

type SramWrite1Wires = (IWire, [IWire], IWire, [OWire])

sramWrite1 :: Word8 -> CircuitBuilder SramWrite1Wires
sramWrite1 n = do
	(wein, weout) <- idGate
	(dcins, dcouts) <- decoder $ 2 ^ n
	(ain1s, ain2s, aouts) <- unzip3 <$> (2 ^ n) `replicateM` andGate
	(din, dout) <- idGate
	(cs, ds, qs, _q_s) <- unzip4 <$> replicateM (2 ^ n) dlatch
	mapM_ (connectWire weout) ain1s
	zipWithM_ connectWire dcouts ain2s
	zipWithM_ connectWire aouts cs
	mapM_ (connectWire dout) ds
	return (wein, dcins, din, qs)

setBitsSramWrite1 ::
	SramWrite1Wires -> Bit -> Word64 -> Bit -> Circuit -> Circuit
setBitsSramWrite1 (wwe, wad, wd, _) bwe ad bd =
	setBit wwe bwe . setBits wad (wordToBits 64 ad) . setBit wd bd

getBitsSramWrite1 :: SramWrite1Wires -> Circuit -> [Bit]
getBitsSramWrite1 (_, _, _, os) cct = (`peekOWire` cct) <$> os

setAndRunSramWrite1 ::
	SramWrite1Wires -> Bit -> Word64 -> Bit -> Int -> Circuit -> Circuit
setAndRunSramWrite1 ws bwe ad bd n = run n . setBitsSramWrite1 ws bwe ad bd

-- sramWrite :: Word8 -> Word8 -> CircuitBuilder (IWire, [IWire], [IWire], [OWire])
