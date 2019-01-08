{-# LANGUAGE ScopedTypeVariables, TypeApplications, BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Bits
import Data.Bool
import Data.Word
import Numeric

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

check :: forall a . Storable a => a -> IO ()
check n = alloca $ \p -> do
	poke p n
--	peek p >>= putStrLn . flip showHex ""
	peekArray (sizeOf (undefined :: a)) (castPtr @_ @Word8 p)
		>>= putStrLn . unwords . map (flip showHex "") . reverse
	peekArray (sizeOf (undefined :: a)) (castPtr @_ @Word8 p)
		>>= putStrLn . unwords . map bits . reverse

bits :: Word8 -> String
bits w = map (bool '0' '1' . (w `testBit`)) [7, 6 .. 0]

test :: [Word8] -> IO Float
test ws = alloca $ \p -> do
	pokeArray (castPtr @_ @Word8 p) $ reverse ws
	peek p

inf, ninf, nan, nan2, nan3, nan4, nan5 :: IO Float
inf = test	[0b01111111, 0b10000000, 0b00000000, 0b00000000]
ninf = test	[0b11111111, 0b10000000, 0b00000000, 0b00000000]
nan = test	[0b11111111, 0b11000000, 0b00000000, 0b00000000]
nan2 = test	[0b11111111, 0b11111111, 0b11111111, 0b11111111]
nan3 = test	[0b01111111, 0b11000000, 0b00000000, 0b00000000]
nan4 = test	[0b01111111, 0b11100000, 0b00000000, 0b00000000]
nan5 = test	[0b01111111, 0b11111111, 0b11111111, 0b11111111]

-- SEEEEEEE EFFFFFFF FFFFFFFF FFFFFFFF
