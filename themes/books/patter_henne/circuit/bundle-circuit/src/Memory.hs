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

instructionMemoryOutput :: RiscvInstMem -> OWire
instructionMemoryOutput = rimOutput

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

type RegisterFileWires = (IWire, IWire, IWire, IWire, IWire, IWire, OWire, OWire, [OWire])

registerFile :: Word8 -> CircuitBuilder RegisterFileWires
registerFile n = do
	(c, w, wadr, d, os) <- registerFileWrite n
	(radr1, r1) <- registerFileReadUnit n os
	(radr2, r2) <- registerFileReadUnit n os
	return (c, radr1, radr2, w, wadr, d, r1, r2, os)

registerFileWrite :: Word8 -> CircuitBuilder (IWire, IWire, IWire, IWire, [OWire])
registerFileWrite n = do
	(cl, ww, cw) <- andGate0
	(wrin, wrout) <- idGate0
	connectWire0 cw wrin
	(adr, dc) <- decoder'' $ fromIntegral n
	(dsin, dsout) <- idGate64
	(wrs, dc', cls) <- unzip3 <$> fromIntegral n `replicateM` andGate0
	(cls', inps, outs) <- unzip3 <$> fromIntegral n `replicateM` register
	connectWire64 wrout `mapM_` wrs
	zipWithM_ connectWire0 dc dc'
	zipWithM_ connectWire0 cls cls'
	connectWire64 dsout `mapM_` inps
	return (cl, ww, adr, dsin, outs)

registerFileReadUnit :: Word8 -> [OWire] -> CircuitBuilder (IWire, OWire)
registerFileReadUnit n os = do
	(adr, ds, r) <- multiplexer $ fromIntegral n
	zipWithM_ connectWire64 os ds
	return (adr, r)

data RiscvRegisterFile = RiscvRegisterFile {
	rrfClock :: IWire,
	rrfReadAddress1 :: IWire, rrfReadAddress2 :: IWire,
	rrfWrite :: IWire, rrfWriteAddress :: IWire, rrfInput :: IWire,
	rrfOutput1 :: OWire, rrfOutput2 :: OWire,
	rrfAllOutputs :: [OWire]
	} deriving Show

riscvRegisterFile :: CircuitBuilder RiscvRegisterFile
riscvRegisterFile = do
	(c, radr1, radr2, w, wadr, d, r1, r2, os) <- registerFile 32
	return $ RiscvRegisterFile c radr1 radr2 w wadr d r1 r2 os

storeRiscvRegisterFile ::
	RiscvRegisterFile -> Word64 -> Word64 -> Circuit -> Circuit
storeRiscvRegisterFile rrf wadr d cct = let
	cct1 = (!! 5) . iterate step
		$ setBits (rrfWriteAddress rrf) (wordToBits wadr) cct
	cct2 = setBits (rrfWrite rrf) (Bits 1)
		. setBits (rrfClock rrf) (Bits 1)
		$ setBits (rrfInput rrf) (wordToBits d) cct1
	cct3 = (!! 10) $ iterate step cct2
	cct4 = (!! 15) . iterate step $ setBits (rrfClock rrf) (Bits 0) cct3
	cct5 = setBits (rrfWrite rrf) (Bits 0) cct4 in
	cct5

readRiscvRegisterFile :: RiscvRegisterFile -> Circuit -> (Word64, Word64)
readRiscvRegisterFile rrf = peekOWire2 (rrfOutput1 rrf) (rrfOutput2 rrf)

debugReadRiscvRegisterFile :: RiscvRegisterFile -> Circuit -> [Word64]
debugReadRiscvRegisterFile = peekMultOWires . rrfAllOutputs

registerFileReadAddress1, registerFileReadAddress2 :: RiscvRegisterFile -> IWire
registerFileReadAddress1 = rrfReadAddress1
registerFileReadAddress2 = rrfReadAddress2
