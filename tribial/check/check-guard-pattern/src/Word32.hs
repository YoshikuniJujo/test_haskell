{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Word32 where

import Data.Bits
import Data.Word

toBytes :: Word32 -> (Word8, Word8, Word8, Word8)
toBytes w = (
	fromIntegral w, fromIntegral $ w `shiftR` 8,
	fromIntegral $ w `shiftR` 16, fromIntegral $ w `shiftR` 24 )

fromBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fromBytes
	(fromIntegral -> w0) (fromIntegral -> w1)
	(fromIntegral -> w2) (fromIntegral -> w3) =
	w0 .|. w1 `shiftL` 8 .|. w2 `shiftL` 16 .|. w3 `shiftL` 24

pattern Bytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
pattern Bytes w0 w1 w2 w3 <- (toBytes -> (w0, w1, w2, w3))
	where Bytes = fromBytes
