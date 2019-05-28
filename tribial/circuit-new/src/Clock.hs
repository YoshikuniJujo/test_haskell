{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Clock where

import Control.Monad
import Data.Word

import Circuit

clock :: Word8 -> CircuitBuilder OWire
clock n | n < 3 = error "clock: span should be more than 2"
clock n = do
	(ni, no) <- notGate
	(dli, dlo) <- delay $ n - 2
	zipWithM_ connectWire [no, dlo] [dli, ni]
	return no

edge :: Word8 -> Word8 -> CircuitBuilder OWire
edge _ n | n < 3 = error "clock: span should be more than 2"
edge m _ | m < 3 = error "edge: edge width should be more than 2"
edge m n = do
	c1 <- clock n
	c2 <- clock n
	(dli, dlo) <- delay (m - 2)
	(ni, no) <- notGate
	(a1, a2, ao) <- andGate
	zipWithM_ connectWire [c2, dlo, no, c1] [dli, ni, a2, a1]
	return ao
