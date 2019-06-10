{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Memory where

import Control.Monad
import Data.List
import Data.Word

import Circuit
import Element
import Clock

sram :: Word8 -> CircuitBuilder (IWire, IWire, IWire, OWire)
sram n = do
	(adrin, adrout) <- idGate64
	(wr, adr, d, qs) <- sramWrite n
	(adr', qs', q) <- triStateSelect n
	connectWire64 adrout `mapM_` [adr, adr']
	zipWithM_ connectWire64 qs qs'
	return (wr, adrin, d, q)

sramWrite :: Word8 -> CircuitBuilder (IWire, IWire, IWire, [OWire])
sramWrite n = do
	(wrin, wrout) <- idGate0
	(din, dout) <- idGate64
	(adr, dec) <- decoder'' n
	(dec', wr', c) <- unzip3 <$> fromIntegral n `replicateM` andGate0
	(c', d, q, _q_) <- unzip4 <$> fromIntegral n `replicateM` dlatch
	connectWire0 wrout `mapM_` wr'
	zipWithM_ connectWire0 dec dec'
	zipWithM_ connectWire0 c c'
	connectWire64 dout `mapM_` d
	return (wrin, adr, din, q)

sramSwitch :: Word8 -> CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire)
sramSwitch n = do
	(sw, radr, wadr, adr) <- mux2
	(wr, adr', d, q) <- sram n
	connectWire (adr, 62, 2) (adr', 62, 0)
	return (sw, wr, radr, wadr, d, q)

data RiscvInstMem = RiscvInstMem {
	rimSwitch :: IWire, rimWrite :: IWire,
	rimReadAddress :: IWire, rimWriteAddress :: IWire,
	rimInput :: IWire, rimOutput :: OWire
	} deriving Show

riscvInstMem :: Word8 -> CircuitBuilder RiscvInstMem
riscvInstMem n = do
	(sw, wr, radr, wadr, d, q) <- sramSwitch n
	return $ RiscvInstMem sw wr radr wadr d q

storeRiscvInstMem :: RiscvInstMem -> Word64 -> Word64 -> Circuit -> Circuit
storeRiscvInstMem rim adr d cct = let
	cct1 = (!! 10) . iterate step $ setMultBits [wsw, wwr, wwadr, wd] [1, 0, adr, d] cct
	cct2 = (!! 20) . iterate step $ setMultBits [wsw, wwr, wwadr, wd] [1, 1, adr, d] cct1
	cct3 = (!! 10) . iterate step $ setMultBits [wsw, wwr, wwadr, wd] [1, 0, adr, d] cct2 in
	cct3
	where
	wsw = rimSwitch rim
	wwr = rimWrite rim
	wwadr = rimWriteAddress rim
	wd = rimInput rim

readRiscvInstMem :: RiscvInstMem -> Circuit -> Word64
readRiscvInstMem rim = bitsToWord . peekOWire (rimOutput rim)

register :: CircuitBuilder (IWire, IWire, OWire)
register = do
	(c, d, q, _q_) <- dflipflop
	return (c, d, q)

data ProgramCounter = ProgramCounter {
	pcClock :: IWire, pcInput :: IWire, pcOutput :: OWire } deriving Show

programCounter :: CircuitBuilder ProgramCounter
programCounter = do
	(c, d, q) <- register
	return $ ProgramCounter c d q

pcClocked :: Clock -> ProgramCounter -> CircuitBuilder ()
pcClocked cl pc = connectWire0 (clockSignal cl) (pcClock pc)

setProgramCounter :: ProgramCounter -> Word64 -> Circuit -> Circuit
setProgramCounter pc w = setBits (pcInput pc) (wordToBits w)
