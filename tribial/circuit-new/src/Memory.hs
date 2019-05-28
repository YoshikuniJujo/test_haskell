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

type SramWrite1Wires = (IWire, [IWire], IWire, [OWire], [OWire])

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
	return (wein, dcins, din, dcouts, qs)

setBitsSramWrite1 ::
	SramWrite1Wires -> Bit -> Word64 -> Bit -> Circuit -> Circuit
setBitsSramWrite1 (wwe, wad, wd, _, _) bwe ad bd =
	setBit wwe bwe . setBits wad (wordToBits 64 ad) . setBit wd bd

getBitsSramWrite1 :: SramWrite1Wires -> Circuit -> [Bit]
getBitsSramWrite1 (_, _, _, _, os) cct = (`peekOWire` cct) <$> os

setAndRunSramWrite1 ::
	SramWrite1Wires -> Bit -> Word64 -> Bit -> Int -> Circuit -> Circuit
setAndRunSramWrite1 ws bwe ad bd n = run n . setBitsSramWrite1 ws bwe ad bd

type SramWrite1'Wires = (IWire, [IWire], IWire, OWire)

sramWrite1' :: Word8 -> CircuitBuilder SramWrite1'Wires
sramWrite1' n = do
	(ad, is, o) <- lazyGate n $ do
		(c, d, q, _q_) <- dlatch
		return ([c, d], q)
	return (is !! 0, ad, is !! 1, o)

setBitsSramWrite1' ::
	SramWrite1'Wires -> Bit -> Word64 -> Bit -> Circuit -> Circuit
setBitsSramWrite1' (wwe, wad, wd, _) bwe ad bd =
	setBit wwe bwe . setBits wad (wordToBits 64 ad) . setBit wd bd

getBitsSramWrite1' :: SramWrite1'Wires -> Circuit -> Bit
getBitsSramWrite1' (_, _, _, o) = peekOWire o

setAndRunSramWrite1' ::
	SramWrite1'Wires -> Bit -> Word64 -> Bit -> Int -> Circuit -> Circuit
setAndRunSramWrite1' ws bwe ad bd n = run n . setBitsSramWrite1' ws bwe ad bd

sramReadPart1 :: Word8 -> [OWire] -> [OWire] -> CircuitBuilder OWire
sramReadPart1 n dc os = do
	(i1s, i2s, tos) <- unzip3 <$> (2 ^ n) `replicateM` triGate
	(rin, rout) <- idGate
	zipWithM_ connectWire dc i1s
	zipWithM_ connectWire os i2s
	mapM_ (`connectWire` rin) tos
	return rout

-- sramWrite :: Word8 -> Word8 -> CircuitBuilder (IWire, [IWire], [IWire], [OWire])

type Sram1Wires = (IWire, [IWire], IWire, OWire)

sram1 :: Word8 -> CircuitBuilder Sram1Wires
sram1 n = do
	(we, ad, d, dc, os) <- sramWrite1 n
	o <- sramReadPart1 n dc os
	return (we, ad, d, o)

setBitsSram1 :: Sram1Wires -> Bit -> Word64 -> Bit -> Circuit -> Circuit
setBitsSram1 (wwe, wad, wd, _) bwe ad bd =
	setBit wwe bwe . setBits wad (wordToBits 64 ad) . setBit wd bd

getBitsSram1 :: Sram1Wires -> Circuit -> Bit
getBitsSram1 (_, _, _, o) = peekOWire o

setAndRunSram1 ::
	Sram1Wires -> Bit -> Word64 -> Bit -> Int -> Circuit -> Circuit
setAndRunSram1 ws bwe ad bd n = run n . setBitsSram1 ws bwe ad bd

setSram1 :: Sram1Wires -> Word64 -> Bit -> Int -> Circuit -> Circuit
setSram1 ws ad bd n = run n . setBitsSram1 ws O ad bd
	. run n . setBitsSram1 ws I ad bd . run n . setBitsSram1 ws O ad bd

sram1' :: Word8 -> CircuitBuilder Sram1Wires
sram1' = sramWrite1'

type SramWires = (IWire, [IWire], [IWire], [OWire])

sram8, sram8' :: Word8 -> CircuitBuilder SramWires
sram8 n = do
	(wein, weout) <- idGate
	(adin, adout) <- unzip <$> fromIntegral n `replicateM` idGate
	(wes, ads, ds, os) <- unzip4 <$> 8 `replicateM` sram1 n
	mapM_ (connectWire weout) wes
	mapM_ (\ad -> zipWithM_ connectWire adout ad) ads
	return (wein, adin, ds, os)

sram8' n = do
	(wein, weout) <- idGate
	(adin, adout) <- unzip <$> fromIntegral n `replicateM` idGate
	(wes, ads, ds, os) <- unzip4 <$> 8 `replicateM` sram1' n
	mapM_ (connectWire weout) wes
	mapM_ (\ad -> zipWithM_ connectWire adout ad) ads
	return (wein, adin, ds, os)

setBitsSram :: SramWires -> Bit -> Word64 -> Word64 -> Circuit -> Circuit
setBitsSram (wwe, wad, wd, _) bwe ad d = setBit wwe bwe
	. setBits wad (wordToBits 64 ad) . setBits wd (wordToBits 64 d)

getBitsSram :: SramWires -> Circuit -> Word64
getBitsSram (_, _, _, os) cct = bitsToWord $ (`peekOWire` cct) <$> os

getBitsSram' :: SramWires -> Circuit -> [Bit]
getBitsSram' (_, _, _, os) cct = (`peekOWire` cct) <$> os

setAndRunSram ::
	SramWires -> Bit -> Word64 -> Word64 -> Int -> Circuit -> Circuit
setAndRunSram ws bwe ad d n = run n . setBitsSram ws bwe ad d

setSram :: SramWires -> Word64 -> Word64 -> Int -> Circuit -> Circuit
setSram ws ad d n = run n . setBitsSram ws O ad d
	. run n . setBitsSram ws I ad d . run n . setBitsSram ws O ad d
