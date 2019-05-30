{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RiscV.Memory (
	Sram, riscvSram, storeSram, loadSram, readSram, readSramBits,
		sramClock, sramAddress, sramInput, sramOutput,
	Register, riscvRegister, registerClock, registerInput, registerOutput,
		resetRegister, readRegisterBits, readRegisterInt
	) where

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

{-
longPress :: Bit -> Word8 -> IWire -> DoCircuit
longPress _ 0 _ = id
longPress b n iw = step . setBit iw b . longPress b (n - 1) iw
-}

longPressWires :: [Bit] -> Word8 -> [IWire] -> DoCircuit
longPressWires _ 0 _ = id
longPressWires bs n iws = step . setBits iws bs . longPressWires bs (n - 1) iws
