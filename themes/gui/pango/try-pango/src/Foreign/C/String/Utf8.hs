{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.String.Utf8 (byteIndices) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Bits

byteIndices :: CStringLen -> IO [Int]
byteIndices = ((0 :) <$>) . go
	where
	go (_, n) | n <= 0 = pure []
	go (p, n) = do
		d <- byteToLen <$> peek p
		(d :) . ((+ d) <$>) <$> go (p `plusPtr` d, n - d)

byteToLen :: Num n => CChar -> n
byteToLen c
	| c .&. 0x80 == 0 = 1
	| c .&. 0xe0 == 0xc0 = 2
	| c .&. 0xf0 == 0xe0 = 3
	| c .&. 0xf8 == 0xf0 = 4
	| otherwise = error "Invalid UTF-8"
