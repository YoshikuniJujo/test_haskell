{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
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
