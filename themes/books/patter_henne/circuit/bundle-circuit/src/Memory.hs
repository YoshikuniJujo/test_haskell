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
	cct1 = (!! 10) . iterate step
		$ setMultBits [wsw, wwr, wwadr, wd] [1, 0, adr, d] cct
	cct2 = (!! 15) . iterate step
		$ setMultBits [wsw, wwr, wwadr, wd] [1, 1, adr, d] cct1
	cct3 = (!! 5) . iterate step
		$ setMultBits [wsw, wwr, wwadr, wd] [1, 0, adr, d] cct2
	cct4 = (!! 5) . iterate step
		$ setMultBits [wsw, wwr, wwadr, wd] [0, 0, adr, d] cct3 in
	cct4
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
	pcSwitch :: IWire, pcManualClock :: IWire, pcManualInput :: IWire,
	pcClock :: IWire, pcInput :: IWire, pcOutput :: OWire } deriving Show

programCounter :: CircuitBuilder ProgramCounter
programCounter = do
	(swin, swout) <- idGate0
	(sw, cc, mc, oc) <- mux2
	(sw', ci, mi, oi) <- mux2
	(c, d, q) <- register
	connectWire0 swout sw
	connectWire0 swout sw'
	connectWire64 oc c
	connectWire64 oi d
	return $ ProgramCounter swin mc mi cc ci q

pcClocked :: Clock -> ProgramCounter -> CircuitBuilder ()
pcClocked cl pc = connectWire0 (clockSignal cl) (pcClock pc)

resetProgramCounter :: ProgramCounter -> Circuit -> Circuit
resetProgramCounter pc = setBits (pcSwitch pc) (Bits 0)
	. foldr (.) id (replicate 10 $ step
		. setBits (pcManualClock pc) (wordToBits 0)
		. setBits (pcManualInput pc) (wordToBits 0))
	. foldr (.) id (replicate 20 $ step
		. setBits (pcManualClock pc) (wordToBits 1)
		. setBits (pcManualInput pc) (wordToBits 0))
	. setBits (pcSwitch pc) (Bits 1)

setProgramCounter :: ProgramCounter -> Word64 -> Circuit -> Circuit
setProgramCounter pc w = setBits (pcInput pc) (wordToBits w)

peekProgramCounter :: ProgramCounter -> Circuit -> Word64
peekProgramCounter pc = bitsToWord . peekOWire (pcOutput pc)
