{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.Memory (
	Sram, riscvSram, riscvSram32, storeSram, loadSram, readSram, readSramBits,
		sramClock, sramAddress, sramInput, sramOutput,
	SramWithSwitch, sramWithSwitch, sramWithSwitchReadAddr, sramWithSwitchOutput,
		storeSramWithSwitch, readSramWithSwitchBits, readSramWithSwitch,
	Register, riscvRegister, registerClock, registerInput, registerOutput,
		resetRegister, readRegisterBits, readRegisterInt,
	RegisterFile, riscvRegisterFile, registerFileReadAddrs,
		registerFileOutput1, registerFileOutput2,
		storeRegisterFile, loadRegisterFile, readRegisterFileBits, readRegisterFile,
	separateRtypeOWires
	) where

import Control.Arrow
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

sram32 :: Word8 -> CircuitBuilder (IWire, [IWire], [IWire], [OWire])
sram32 n = do
	(wein, weout) <- idGate
	(adin, adout) <- unzip <$> fromIntegral n `replicateM` idGate
	(wes, ads, ds, os) <- unzip4 <$> 32 `replicateM` sram1 n
	mapM_ (connectWire weout) wes
	mapM_ (\ad -> zipWithM_ connectWire adout ad) ads
	return (wein, adin, ds, os)

data Sram = Sram IWire [IWire] [IWire] [OWire]

riscvSram :: CircuitBuilder Sram
riscvSram = do
	(we, ad, ds, os) <- sram8 64
	return $ Sram we ad ds os

riscvSram32 :: CircuitBuilder Sram
riscvSram32 = do
	(we, ad, ds, os) <- sram32 62
	return $ Sram we ad ds os

sramClock :: Sram -> IWire
sramClock (Sram c _ _ _) = c

sramAddress :: Sram -> [IWire]
sramAddress (Sram _ a _ _) = a

sramInput :: Sram -> [IWire]
sramInput (Sram _ _ i _) = i

sramOutput :: Sram -> [OWire]
sramOutput (Sram _ _ _ o) = o

storeSram :: Sram -> Int64 -> Word8 -> DoCircuit
storeSram (Sram wwe wad wb _) ad b = run 2
	. setBit wwe O . setBits wad (numToBits 64 ad)
	. setBits wb (numToBits 8 b) . run 4
	. setBit wwe I . setBits wad (numToBits 64 ad)
	. setBits wb (numToBits 8 b) . run 3
	. setBit wwe O . setBits wad (numToBits 64 ad)
	. setBits wb (numToBits 8 b)

loadSram :: Sram -> Int64 -> DoCircuit
loadSram (Sram wwe wad _ _) ad = run 20
	. setBit wwe O . setBits wad (numToBits 64 ad)

readSram :: Sram -> Circuit -> Word8
readSram (Sram _ _ _ os) cct = bitsToNum $ (`peekOWire` cct) <$> os

readSramBits :: Sram -> Circuit -> [Bit]
readSramBits (Sram _ _ _ os) cct = (`peekOWire` cct) <$> os

data SramWithSwitch = SramWithSwitch IWire IWire [IWire] [IWire] [IWire] [OWire]

sramWithSwitch :: CircuitBuilder SramWithSwitch
sramWithSwitch = do
	(slin, slout) <- idGate
	(rasin, rasout) <- unzip <$> 64 `replicateM` idGate
	(ss, ras, was, as) <- unzip4 <$> 64 `replicateM` mux2
	(we, ad, ds, os) <- sram32 62
	connectWire slout `mapM_` ss
	zipWithM_ connectWire (tail $ tail rasout) ras
	zipWithM_ connectWire as ad
	return $ SramWithSwitch we slin rasin was ds os

sramWithSwitchReadAddr :: SramWithSwitch -> [IWire]
sramWithSwitchReadAddr (SramWithSwitch _ _ ra _ _ _) = ra

sramWithSwitchOutput :: SramWithSwitch -> [OWire]
sramWithSwitchOutput (SramWithSwitch _ _ _ _ _ o) = o

storeSramWithSwitch :: SramWithSwitch -> Int64 -> Word32 -> DoCircuit
storeSramWithSwitch (SramWithSwitch we sw _ was wds _) ad d = run 3 . setBit sw O . run 5
	. setBit we O . setBits was (numToBits 64 ad) . setBits wds (numToBits 32 d) . run 8
	. setBit we I . setBits was (numToBits 64 ad) . setBits wds (numToBits 32 d) . run 8
	. setBit we O . setBits was (numToBits 64 ad) . setBits wds (numToBits 32 d) . setBit sw I

readSramWithSwitchBits :: SramWithSwitch -> Circuit -> [Bit]
readSramWithSwitchBits (SramWithSwitch _ _ _ _ _ os) cct = (`peekOWire` cct) <$> os

readSramWithSwitch :: SramWithSwitch -> Circuit -> Word32
readSramWithSwitch sr = bitsToNum . readSramWithSwitchBits sr

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

data Register = Register IWire [IWire] [OWire] deriving Show

riscvRegister :: CircuitBuilder Register
riscvRegister = do
	(c, ds, qs) <- register
	return $ Register c ds qs

registerClock :: Register -> IWire
registerClock (Register c _ _) = c

registerInput :: Register -> [IWire]
registerInput (Register _ ds _) = ds

registerOutput :: Register -> [OWire]
registerOutput (Register _ _ qs) = qs

readRegisterBits :: Register -> Circuit -> [Bit]
readRegisterBits (Register _ _ qs) cct = (`peekOWire` cct) <$> qs

readRegisterInt :: Register -> Circuit -> Int64
readRegisterInt rg = bitsToNum . readRegisterBits rg

resetRegister :: Register -> Circuit -> Circuit
resetRegister rg = longPressWires (repeat O) 20 (registerInput rg)
	. longPressWires (I : repeat O) 20 (registerClock rg : registerInput rg)
	. longPressWires (repeat O) 20 (registerInput rg)

longPressWires :: [Bit] -> Word8 -> [IWire] -> DoCircuit
longPressWires _ 0 _ = id
longPressWires bs n iws = step . setBits iws bs . longPressWires bs (n - 1) iws

registerFileReadUnit ::
	Word8 -> Word8 -> [[OWire]] -> CircuitBuilder ([IWire], [OWire])
registerFileReadUnit n m oss = do
	(adr, ds, r) <- multiplexer n (fromIntegral m)
	zipWithM_ (zipWithM_ connectWire) oss ds
	return (adr, r)

type RegisterFileWires = ([IWire], [IWire], IWire, [IWire], [IWire], [OWire], [OWire])

registerFile :: Word8 -> Word8 -> CircuitBuilder RegisterFileWires
registerFile n m = do
	(wrin, wrout) <- idGate
	(wadr, dc) <- decoder $ fromIntegral m
	(dsin, dsout) <- unzip <$> fromIntegral n `replicateM` idGate
	(wrs, dc', cls) <- unzip3 <$> fromIntegral m `replicateM` andGate
	(cls', inps, outs) <- unzip3 <$> fromIntegral m `replicateM` register
	connectWire wrout `mapM_` wrs
	zipWithM_ connectWire dc dc'
	zipWithM_ connectWire cls cls'
	zipWithM_ connectWire dsout `mapM_` inps
	(radr1, out1) <- registerFileReadUnit n m outs
	(radr2, out2) <- registerFileReadUnit n m outs
	return (radr1, radr2, wrin, wadr, dsin, out1, out2)

data RegisterFile = RegisterFile [IWire] [IWire] IWire [IWire] [IWire] [OWire] [OWire]

riscvRegisterFile :: Word8 -> Word8 -> CircuitBuilder RegisterFile
riscvRegisterFile n m = do
	(radr1, radr2, wr, wadr, wdt, out1, out2) <- registerFile n m
	return $ RegisterFile radr1 radr2 wr wadr wdt out1 out2

registerFileReadAddrs :: RegisterFile -> ([IWire], [IWire])
registerFileReadAddrs (RegisterFile radr1 radr2 _ _ _ _ _) = (radr1, radr2)

registerFileOutput1 :: RegisterFile -> [OWire]
registerFileOutput1 (RegisterFile _ _ _ _ _ o1 _) = o1

registerFileOutput2 :: RegisterFile -> [OWire]
registerFileOutput2 (RegisterFile _ _ _ _ _ _ o2) = o2

storeRegisterFile :: RegisterFile -> Word8 -> Int64 -> DoCircuit
storeRegisterFile (RegisterFile _ _ c wadr wdt _ _) adr dt = run 20 . setBit c O
	. run 20 . setBit c I . run 20
	. setBits wadr (numToBits 8 adr) . setBits wdt (numToBits 64 dt)

loadRegisterFile :: RegisterFile -> Word8 -> Word8 -> DoCircuit
loadRegisterFile (RegisterFile wr1 wr2 _ _ _ _ _) r1 r2 =
	run 30 . setBits wr1 (numToBits 8 r1) . setBits wr2 (numToBits 8 r2)

readRegisterFileBits :: RegisterFile -> Circuit -> ([Bit], [Bit])
readRegisterFileBits (RegisterFile _ _ _ _ _ o1 o2) =
	(,) <$> peekOWires o1 <*> peekOWires o2

readRegisterFile :: RegisterFile -> Circuit -> (Word64, Word64)
readRegisterFile rf = (bitsToNum *** bitsToNum) . readRegisterFileBits rf

type SeparatedRtype a = ([a], ([a], [a]), [a], [a], [a])

separateRtypeXs :: [a] -> SeparatedRtype a
separateRtypeXs xs = let
	(oc, xs2) = splitAt 7 xs
	(rd, xs3) = splitAt 5 xs2
	(f3, xs4) = splitAt 3 xs3
	(r2, xs5) = splitAt 5 xs4
	(r1, xs6) = splitAt 5 xs5
	(f7, []) = splitAt 7 xs6 in
	(f7, (r1, r2), f3, rd, oc)

separateRtypeOWires :: [OWire] -> SeparatedRtype OWire
separateRtypeOWires = separateRtypeXs
