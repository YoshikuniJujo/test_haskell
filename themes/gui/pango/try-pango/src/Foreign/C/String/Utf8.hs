{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.String.Utf8 where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Bits

byteToLen :: Num n => CChar -> n
byteToLen c
	| c .&. 0x80 == 0 = 1
	| c .&. 0xe0 == 0xc0 = 2
	| c .&. 0xf0 == 0xe0 = 3
	| c .&. 0xf8 == 0xf0 = 4
	| otherwise = error "Invalid UTF-8"

byteIndices :: CStringLen -> IO [Int]
byteIndices (_, n) | n <= 0 = pure []
byteIndices (p, n) = do
	d <- byteToLen <$> peek p
	(d :) . ((+ d) <$>) <$> byteIndices (p `plusPtr` d, n - d)
