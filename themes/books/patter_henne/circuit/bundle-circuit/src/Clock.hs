{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Clock where

import Data.Word

import Circuit
import Element

data Clock = Clock { clkSwitch :: IWire,  clkSignal :: OWire } deriving Show

clock :: Word8 -> CircuitBuilder Clock
clock n = uncurry Clock <$> clockGen n

clockOn :: Clock -> Circuit -> Circuit
clockOn cl = setBits (clkSwitch cl) (Bits 1)

clockSignal :: Clock -> OWire
clockSignal = clkSignal

clockGen :: Word8 -> CircuitBuilder (IWire, OWire)
clockGen n = do
	(i, o) <- notGate0
	(clsw, cloff, clon, swo) <- mux2
	z <- constGate0 (Bits 1)
	connectWire0 z cloff
	connectWire0 o clon
	connectWire0 swo i
	delay i n
	return (clsw, o)
