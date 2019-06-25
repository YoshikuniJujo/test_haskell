{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Memory where

import Control.Monad
import Data.List
import Data.Word

import Circuit
import Element
import Clock

sram :: Word8 -> CircuitBuilder (IWire, IWire, IWire, OWire, [OWire])
sram n = do
	(adrin, adrout) <- idGate64
	(wr, adr, d, qs) <- sramWrite n
	(adr', qs', q) <- triStateSelect n
	connectWire64 adrout `mapM_` [adr, adr']
	zipWithM_ connectWire64 qs qs'
	return (wr, adrin, d, q, qs)

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
	(wr, adr', d, q, _) <- sram n
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
	cct1 = (!! 15) . iterate step
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

data RiscvDataMem = RiscvDataMem {
	rdmClock :: IWire,
	rdmWrite :: IWire,
	rdmRead :: IWire,
	rdmAddress :: IWire,
	rdmInput :: IWire,
	rdmOutput :: OWire,

	rdmSwitch :: IWire,
	rdmOuterClock :: IWire,
	rdmOuterWrite :: IWire,
	rdmOuterAddress :: IWire,
	rdmOuterInput :: IWire,
	rdmDebugOutput :: [OWire]
	} deriving Show

riscvDataMem :: Word8 -> CircuitBuilder RiscvDataMem
riscvDataMem n = do
	(rd, ob, oo) <- orGate0
	(s, _zero, adrin, adrout) <- mux2
	(aa, ab, ao) <- andGate0

	(swin, swout) <- idGate0
	(c, fe) <- fallingEdge 7
	(swc, inrc, outc, cout) <- mux2
	connectWire0 swout swc
	connectWire0 cout c
--	(wrin, wrout) <- idGate0
	(sww, inwr, outwr, wrout) <- mux2
	connectWire0 swout sww
--	connectWire0 wrout wrin
	(sw0, adr, oadr, adro) <- mux2
	(sw1, d, od, dto) <- mux2
	(wr, adr', d', q, aq) <- sram n
	connectWire0 swout `mapM_` [sw0, sw1]
	connectWire (adro, 61, 3) (adr', 61, 0)
	connectWire64 dto d'

	connectWire0 wrout ob
	connectWire0 oo s
	connectWire64 adrout adr
	connectWire0 fe aa
	connectWire0 wrout ab
	connectWire0 ao wr
	return $ RiscvDataMem inrc inwr rd adrin d q swin outc outwr oadr od aq

dataMemClock :: RiscvDataMem -> IWire
dataMemClock = rdmClock

storeRiscvDataMem :: RiscvDataMem -> Word64 -> Word64 -> Circuit -> Circuit
storeRiscvDataMem rdm adr d cct = let
	cct1 = (!! 15) . iterate step
		$ setMultBits [sw, wcl, wwr, wwadr, wd] [1, 0, 1, adr, d] cct
	cct2 = (!! 15) . iterate step
		$ setMultBits [sw, wcl, wwr, wwadr, wd] [1, 0, 1, adr, d] cct1
	cct3 = (!! 15) . iterate step
		$ setMultBits [sw, wcl, wwr, wwadr, wd] [1, 1, 1, adr, d] cct2
	cct4 = (!! 5) . iterate step
		$ setMultBits [sw, wcl, wwr, wwadr, wd] [1, 0, 1, adr, d] cct3
	cct5 = (!! 15) . iterate step
		$ setMultBits [sw, wcl, wwr, wwadr, wd] [1, 0, 0, adr, d] cct4
	cct6 = (!! 10) . iterate step
		$ setMultBits [sw, wcl, wwr, wwadr, wd] [0, 0, 0, adr, d] cct5 in
	cct6
	where
	sw = rdmSwitch rdm
	wcl = rdmOuterClock rdm
	wwr = rdmOuterWrite rdm
	wwadr = rdmOuterAddress rdm
	wd = rdmOuterInput rdm

registerGen :: CircuitBuilder (IWire, IWire, OWire)
registerGen = do
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
	(c, d, q) <- registerGen
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

stopProgramCounter :: ProgramCounter -> RiscvRegisterFile -> Word16 -> Circuit -> Circuit
stopProgramCounter pc rf n_ = setBits (pcSwitch pc) (Bits 0) . setBits (rrfSwitch rf) (Bits 0)
	. (!! n) . iterate step . setBits (pcSwitch pc) (Bits 1) . setBits (rrfSwitch rf) (Bits 1)
	. setBits (rrfOuterClock rf) (Bits 1)
	. setBits (pcManualClock pc) (Bits 1)
	where
	n = fromIntegral n_

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
	(cls', inps, outs) <- unzip3 <$> fromIntegral n `replicateM` registerGen
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

	rrfSwitch :: IWire,
	rrfOuterClock :: IWire,
	rrfOuterWrite :: IWire,
	rrfOuterWriteAddress :: IWire,
	rrfOuterInput :: IWire,
	rrfAllOutputs :: [OWire]
	} deriving Show

riscvRegisterFile :: CircuitBuilder RiscvRegisterFile
riscvRegisterFile = do
	(cl, radr1, radr2, wr, wadr, dt, r1, r2, os) <- registerFile 32
	(swin, swout) <- idGate0
	(sw0, c, c', co) <- mux2
	(sw1, w, w', wo) <- mux2
	(sw2, wa, wa', wao) <- mux2
	(sw3, d, d', dto) <- mux2
	connectWire0 swout `mapM_` [sw0, sw1, sw2, sw3]
	zipWithM_ connectWire64 [co, wo, wao, dto] [cl, wr, wadr, dt]
	return $ RiscvRegisterFile c radr1 radr2 w wa d r1 r2 swin c' w' wa' d' os

storeRiscvRegisterFile ::
	RiscvRegisterFile -> Word64 -> Word64 -> Circuit -> Circuit
storeRiscvRegisterFile rrf wadr d cct = let
	cct0 = (!! 5) . iterate step $ setBits (rrfSwitch rrf) (Bits 1) cct
	cct1 = (!! 5) . iterate step
		$ setBits (rrfOuterWriteAddress rrf) (wordToBits wadr) cct0
	cct2 = setBits (rrfOuterWrite rrf) (Bits 1)
		. setBits (rrfOuterClock rrf) (Bits 1)
		$ setBits (rrfOuterInput rrf) (wordToBits d) cct1
	cct3 = (!! 10) $ iterate step cct2
	cct4 = (!! 15) . iterate step $ setBits (rrfOuterClock rrf) (Bits 0) cct3
	cct5 = setBits (rrfOuterWrite rrf) (Bits 0) cct4
	cct6 = (!! 5) . iterate step $ setBits (rrfSwitch rrf) (Bits 0) cct5 in
	cct6

readRiscvRegisterFile :: RiscvRegisterFile -> Circuit -> (Word64, Word64)
readRiscvRegisterFile rrf = peekOWire2 (rrfOutput1 rrf) (rrfOutput2 rrf)

debugReadRiscvRegisterFile :: RiscvRegisterFile -> Circuit -> [Word64]
debugReadRiscvRegisterFile = peekMultOWires . rrfAllOutputs

registerFileReadAddress1, registerFileReadAddress2 :: RiscvRegisterFile -> IWire
registerFileReadAddress1 = rrfReadAddress1
registerFileReadAddress2 = rrfReadAddress2

registerFileWriteAddress :: RiscvRegisterFile -> IWire
registerFileWriteAddress = rrfWriteAddress

data Register = Register {
	rgSwitch :: IWire, rgManualClock :: IWire, rgManualInput :: IWire,
	rgClock :: IWire, rgInput :: IWire, rgOutput :: OWire }
	deriving Show

registerClock, registerInput :: Register -> IWire
registerClock = rgClock
registerInput = rgInput

registerOutput :: Register -> OWire
registerOutput = rgOutput

register :: CircuitBuilder Register
register = do
	(swin, swout) <- idGate0
	(sw, cc, mc, oc) <- mux2
	(sw', ci, mi, oi) <- mux2
	(c, d, q, _q_) <- dflipflop
	connectWire0 swout sw
	connectWire0 swout sw'
	connectWire0 oc c
	connectWire64 oi d
	return $ Register swin mc mi cc ci q

resetRegister :: Register -> Circuit -> Circuit
resetRegister rg cct = let
	cct1 = (!! 10) . iterate step $ setBits (rgSwitch rg) (Bits 1) cct
	cct2 = (!! 20) . iterate step
		. setBits (rgManualInput rg) (Bits 0)
		$ setBits (rgManualClock rg) (Bits 1) cct1
	cct3 = (!! 20) . iterate step $ setBits (rgManualClock rg) (Bits 0) cct2
	cct4 = (!! 10) . iterate step $ setBits (rgSwitch rg) (Bits 0) cct3 in
	cct4
