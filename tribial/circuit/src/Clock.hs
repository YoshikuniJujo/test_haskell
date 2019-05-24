{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Clock where

import Control.Monad
import Data.Word

import Circuit

clock :: Word8 -> CircuitBuilder OWire
clock n | n < 3 = error "clock: span should be more then 2"
clock n = do
	(ni, no) <- notGate
	(dli, dlo) <- delay $ n - 2
	zipWithM_ connectWire [no, dlo] [dli, ni]
	return no
