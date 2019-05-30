{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Clock (clock, reset) where

import Control.Monad
import Data.Word

import Circuit

data Clock = Clock Word8 IWire OWire deriving Show

clock :: Word8 -> CircuitBuilder Clock
clock n = do
	(ci, co) <- makeClock n
	return $ Clock n ci co

reset :: Clock -> DoCircuit
reset (Clock n ci co) = resetClock n (ci, co)

makeClock :: Word8 -> CircuitBuilder (IWire, OWire)
makeClock n | n < 3 = error "clock: span should be more than 2"
makeClock n = do
	(ni, no) <- notGate
	(dli, dlo) <- delay $ n - 2
	zipWithM_ connectWire [no, dlo] [dli, ni]
	return (ni, no)

resetClock :: Word8 -> (IWire, OWire) -> Circuit -> Circuit
resetClock 0 _ = id
resetClock n ws@(cr, _) = step . setBit cr O . resetClock (n - 1) ws
