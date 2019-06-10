{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Clock where

import Data.Word

import Circuit

data Clock = Clock { clkReseter :: IWire,  clkSignal :: OWire } deriving Show

clock :: Word8 -> CircuitBuilder Clock
clock n = uncurry Clock <$> clockGen n

resetClock :: Clock -> Circuit -> Circuit
resetClock cl = setBits (clkReseter cl) (Bits 0)

clockSignal :: Clock -> OWire
clockSignal = clkSignal

clockGen :: Word8 -> CircuitBuilder (IWire, OWire)
clockGen n = do
	(i, o) <- notGate0
	connectWire0 o i
	delay i n
	return (i, o)
