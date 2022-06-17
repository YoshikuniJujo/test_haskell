{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Bits
import Data.Maybe
import Data.Bool

toBits :: FiniteBits a => a -> [a]
toBits x =
	catMaybes $ checkBit x . (bit 0 `shiftL`) <$> [0 .. finiteBitSize x - 1]

checkBit :: FiniteBits a => a -> a -> Maybe a
checkBit bs b = bool Nothing (Just b) $ bs .&. b /= zeroBits
