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
	(ad, is, o) <- lazyGates n $ do
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

type SramXyWrite1Wires = (IWire, [IWire], [IWire], IWire, OWire)

sramXyWrite1 :: Word8 -> Word8 -> CircuitBuilder SramXyWrite1Wires
sramXyWrite1 m n = do
	(ady, wedadx, o) <- lazyGates m $ do
		(iwe, adx, idt, io) <- sramXWrite1 n
		return (iwe : idt : adx, io)
	case wedadx of
		we : d : adx -> return (we, ady, adx, d, o)
		_ -> error "Oops!"

sramXWrite1 :: Word8 -> CircuitBuilder (IWire, [IWire], IWire, OWire)
sramXWrite1 n = do
	(ad, is, o) <- lazyGates n $ do
		(c, d, q, _q_) <- dlatch
		return ([c, d], q)
	return (is !! 0, ad, is !! 1, o)

setBitsSramXyWrite1 :: SramXyWrite1Wires ->
	Bit -> Word64 -> Word64 -> Bit -> Circuit -> Circuit
setBitsSramXyWrite1 (wwe, wady, wadx, wd, _) bwe ady adx bd = setBit wwe bwe
	. setBits wady (wordToBits 64 ady) . setBits wadx (wordToBits 64 adx)
	. setBit wd bd

getBitsSramXyWrite1 :: SramXyWrite1Wires -> Circuit -> Bit
getBitsSramXyWrite1 (_, _, _, _, o) = peekOWire o

setAndRunSramXyWrite1 :: SramXyWrite1Wires ->
	Bit -> Word64 -> Word64 -> Bit -> Int -> Circuit -> Circuit
setAndRunSramXyWrite1 ws bwe ady adx bd n = run n . setBitsSramXyWrite1 ws bwe ady adx bd

setSramXyWrite1 :: SramXyWrite1Wires ->
	Word64 -> Word64 -> Bit -> Int -> Circuit -> Circuit
setSramXyWrite1 ws ady adx d n = setAndRunSramXyWrite1 ws O ady adx d n
	. setAndRunSramXyWrite1 ws I ady adx d n
	. setAndRunSramXyWrite1 ws O ady adx d n

type SramXy1Wires = (IWire, [IWire], [IWire], IWire, OWire)

sramXy1 :: Word8 -> Word8 -> CircuitBuilder SramXy1Wires
sramXy1 = sramXyWrite1

type SramXyWires = (IWire, [IWire], [IWire], [IWire], [OWire])

sramXy8 :: Word8 -> Word8 -> CircuitBuilder SramXyWires
sramXy8 m n = do
	(wein, weout) <- idGate
	(adyin, adyout) <- unzip <$> fromIntegral m `replicateM` idGate
	(adxin, adxout) <- unzip <$> fromIntegral n `replicateM` idGate
	(wes, adys, adxs, ds, os) <- unzip5 <$> 8 `replicateM` sramXy1 m n
	mapM_ (connectWire weout) wes
	mapM_ (\ady -> zipWithM_ connectWire adyout ady) adys
	mapM_ (\adx -> zipWithM_ connectWire adxout adx) adxs
	return (wein, adyin, adxin, ds, os)

setBitsSramXy ::
	SramXyWires -> Bit -> Word64 -> Word64 -> Word64 -> Circuit -> Circuit
setBitsSramXy (wwe, wady, wadx, wd, _) bwe ady adx d = setBit wwe bwe
	. setBits wady (wordToBits 64 ady) . setBits wadx (wordToBits 64 adx)
	. setBits wd (wordToBits 64 d)

getBitsSramXy :: SramXyWires -> Circuit -> Word64
getBitsSramXy (_, _, _, _, os) cct = bitsToWord $ (`peekOWire` cct) <$> os

setAndRunSramXy :: SramXyWires ->
	Bit -> Word64 -> Word64 -> Word64 -> Int -> Circuit -> Circuit
setAndRunSramXy ws bwe ady adx d n = run n . setBitsSramXy ws bwe ady adx d

setSramXy ::
	SramXyWires -> Word64 -> Word64 -> Word64 -> Int -> Circuit -> Circuit
setSramXy ws ady adx d n = setAndRunSramXy ws O ady adx d n
	. setAndRunSramXy ws I ady adx d n
	. setAndRunSramXy ws O ady adx d n

getSramXy ::
	SramXyWires -> Word64 -> Word64 -> Word64 -> Int -> Circuit -> Circuit
getSramXy ws ady adx d n = setAndRunSramXy ws O ady adx d n

---------------------------------------------------------------------------

-- type SramWrite1'Wires = (IWire, [IWire], IWire, OWire)

sram1'' :: Word8 -> CircuitBuilder SramWrite1'Wires
sram1'' n = do
	(ad, is, o) <- strictGates n $ do
		(c, d, q, _q_) <- dlatch
		return ([c, d], q)
	return (is !! 0, ad, is !! 1, o)

---------------------------------------------------------------------------

type DflipflopWires = (IWire, IWire, OWire, OWire)

dflipflop :: CircuitBuilder DflipflopWires
dflipflop = do
	(c0, d0, q0, _) <- dlatch
	(c1, d1, q1, q_1) <- dlatch
	(cin, cout) <- idGate
	(ni, no) <- notGate
	zipWithM_ connectWire [cout, cout, no, q0] [c0, ni, c1, d1]
	return (cin, d0, q1, q_1)

setDflipflop :: DflipflopWires -> Bit -> DoCircuit
setDflipflop (c, d, _, _) b =
	run 20 . setBit c O . run 20 . setBit c I . run 20 . setBit d b

setDataDflipflop :: DflipflopWires -> Bit -> DoCircuit
setDataDflipflop (_, d, _, _) b = run 20 . setBit d b

readDflipflop :: DflipflopWires -> Circuit -> (Bit, Bit)
readDflipflop (_, _, q, q_) = (,) <$> peekOWire q <*> peekOWire q_

type RegisterWires = (IWire, [IWire], [OWire])

registerN :: Word8 -> CircuitBuilder RegisterWires
registerN n = do
	(cin, cout) <- idGate
	(cs, ds, qs, _) <- unzip4 <$> fromIntegral n `replicateM` dflipflop
	mapM_ (connectWire cout) cs
	return (cin, ds, qs)

setRegister :: RegisterWires -> Word64 -> DoCircuit
setRegister (c, ds, _) d = run 20 . setBit c O
	. run 20 . setBit c I . run 20 . setBits ds (wordToBits 64 d) . run 20

setDataRegister :: RegisterWires -> Word64 -> DoCircuit
setDataRegister (_, ds, _) d = run 20 . setBits ds (wordToBits 64 d)

readRegister :: RegisterWires -> Circuit -> Word64
readRegister (_, _, qs) cct = bitsToWord $ (`peekOWire` cct) <$> qs

type RegisterFileWriteWires = (IWire, [IWire], [IWire], [[OWire]])

registerFileWrite :: Word8 -> Word8 -> CircuitBuilder RegisterFileWriteWires
registerFileWrite n m = do
	(wrin, wrout) <- idGate
	(adr, dc) <- decoder $ fromIntegral m
	(dsin, dsout) <- unzip <$> fromIntegral n `replicateM` idGate
	(wrs, dc', cls) <- unzip3 <$> fromIntegral m `replicateM` andGate
	(cls', inps, outs) <- unzip3 <$> fromIntegral m `replicateM` registerN n
	connectWire wrout `mapM_` wrs
	zipWithM_ connectWire dc dc'
	zipWithM_ connectWire cls cls'
	mapM_ (zipWithM_ connectWire dsout) inps
	return (wrin, adr, dsin, outs)

setRegisterFileWrite :: RegisterFileWriteWires -> Word64 -> Word64 -> DoCircuit
setRegisterFileWrite (c, wadr, wds, _) adr d = run 20 . setBit c O
	. run 20 . setBit c I . run 20
	. setBits wadr (wordToBits 64 adr) . setBits wds (wordToBits 64 d)

readRegisterFileWrite :: RegisterFileWriteWires -> Circuit -> [Maybe Word64]
readRegisterFileWrite (_, _, _, woss) cct =
	bitsToWordMaybe . (`peekOWires` cct) <$> woss
