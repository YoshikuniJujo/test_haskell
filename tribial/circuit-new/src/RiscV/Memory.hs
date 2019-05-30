{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.Memory (
	Sram, riscvSram, storeSram, loadSram, readSram, register ) where

import Control.Monad
import Data.List
import Data.Word
import Data.Int

import Circuit
import CircuitTools
import RiscV.Element
import RiscV.Tools

rslatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
rslatch = do
	(r, q_', q) <- norGate
	(s, q', q_) <- norGate
	zipWithM_ connectWire [q, q_] [q', q_']
	return (r, s, q, q_)

dlatch :: CircuitBuilder (IWire, IWire, OWire, OWire)
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

sram1 :: Word8 -> CircuitBuilder (IWire, [IWire], IWire, OWire)
sram1 n = do
	(ad, is, o) <- lazyGates n $ do
		(c, d, q, _q_) <- dlatch
		return ([c, d], q)
	return (is !! 0, ad, is !! 1, o)

sram8 :: Word8 -> CircuitBuilder (IWire, [IWire], [IWire], [OWire])
sram8 n = do
	(wein, weout) <- idGate
	(adin, adout) <- unzip <$> fromIntegral n `replicateM` idGate
	(wes, ads, ds, os) <- unzip4 <$> 8 `replicateM` sram1 n
	mapM_ (connectWire weout) wes
	mapM_ (\ad -> zipWithM_ connectWire adout ad) ads
	return (wein, adin, ds, os)

data Sram = Sram IWire [IWire] [IWire] [OWire]

riscvSram :: CircuitBuilder Sram
riscvSram = do
	(we, ad, ds, os) <- sram8 64
	return $ Sram we ad ds os

storeSram :: Sram -> Int64 -> Word8 -> DoCircuit
storeSram (Sram wwe wad wb _) ad b = run 20
	. setBit wwe O . setBits wad (numToBits 64 ad)
	. setBits wb (numToBits 8 b) . run 20
	. setBit wwe I . setBits wad (numToBits 64 ad)
	. setBits wb (numToBits 8 b) . run 20
	. setBit wwe O . setBits wad (numToBits 64 ad)
	. setBits wb (numToBits 8 b)

loadSram :: Sram -> Int64 -> DoCircuit
loadSram (Sram wwe wad _ _) ad = run 20
	. setBit wwe O . setBits wad (numToBits 64 ad)

readSram :: Sram -> Circuit -> Word8
readSram (Sram _ _ _ os) cct = bitsToNum $ (`peekOWire` cct) <$> os

dflipflop :: CircuitBuilder (IWire, IWire, OWire, OWire)
dflipflop = do
	(c0, d0, q0, _) <- dlatch
	(c1, d1, q1, q_1) <- dlatch
	(cin, cout) <- idGate
	(ni, no) <- notGate
	zipWithM_ connectWire [cout, cout, no, q0] [c0, ni, c1, d1]
	return (cin, d0, q1, q_1)

register :: CircuitBuilder (IWire, [IWire], [OWire])
register = do
	(cin, cout) <- idGate
	(cs, ds, qs, _) <- unzip4 <$> 64 `replicateM` dflipflop
	connectWire cout `mapM_` cs
	return (cin, ds, qs)
