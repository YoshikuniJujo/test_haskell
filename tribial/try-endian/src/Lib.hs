{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.Bits
import Data.Word
import System.IO.Unsafe

b1234 :: IO Word32
b1234 = alloca \p -> do
	let	bs = (castPtr p `plusPtr`) <$> [0, 1, 2, 3] :: [Ptr Word8]
	zipWithM_ poke bs [1 ..]
	peek p

data Endian = LittleEndian | BigEndian | UnknownEndian deriving Show

endian32 :: Endian
endian32 = unsafePerformIO $ b1234 >>= \case
	0x04030201 -> pure LittleEndian
	0x01020304 -> pure BigEndian
	_ -> pure UnknownEndian

bitIndices32 :: [Int]
bitIndices32 = case endian32 of
	LittleEndian -> [0 .. 31]
	BigEndian -> [31, 30 .. 0]
	UnknownEndian -> error "UnknownEndian"

word32ToBools :: Word32 -> [Bool]
word32ToBools w = (w `testBit`) <$> bitIndices32

set :: Bits a => Int -> Bool -> a -> a
set n False x = x `clearBit` n
set n True x = x `setBit` n

boolsToWord32 :: [Bool] -> Word32
boolsToWord32 bs = foldr (.) id (zipWith set bitIndices32 bs) 0
