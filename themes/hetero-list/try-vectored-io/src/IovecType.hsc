{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IovecType (Iovec(..)) where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..), pokeByteOff)
import Foreign.C.Types (CChar, CSize)

#include <sys/uio.h>

data {-# CTYPE "sys/uio.h" "struct iovec" #-} Iovec = Iovec (Ptr CChar) CSize

instance Storable Iovec where
	sizeOf _ = #size struct iovec
	alignment _ = #alignment struct iovec
	peek p = Iovec <$> #{peek struct iovec, iov_base} p <*> #{peek struct iovec, iov_len} p
	poke p (Iovec b l) =
		#{poke struct iovec, iov_base} p b >> #{poke struct iovec, iov_len} p l
