{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iovec (
	withIovec, Iovec, PluralPtrLen(..), PtrLenList(..), PtrLenTuple(..), ListTuple(..)
	) where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import Foreign.Marshal (allocaBytes)
import Foreign.C.Types (CInt, CChar)
import Control.Arrow (second)
import Data.List (genericLength)

import PluralPtrLen (
	PluralPtrLen(..), toCCharPtrLenList, PtrLenList(..), PtrLenTuple(..), ListTuple(..) )

withIovec :: PluralPtrLen pl => pl -> (Ptr Iovec -> CInt -> IO a) -> IO a
withIovec = c_withIovec . (second fromIntegral <$>) . toCCharPtrLenList

#include <sys/uio.h>

data {-# CTYPE "sys/uio.h" "struct iovec" #-} Iovec

c_withIovec :: [(Ptr CChar, CInt)] -> (Ptr Iovec -> CInt -> IO a) -> IO a
c_withIovec pns act = allocaBytes (ln * #{size struct iovec}) $ \piov ->
	c_prepareIovec piov pns >> act piov ln
	where ln :: Num n => n; ln = genericLength pns

c_prepareIovec :: Ptr Iovec -> [(Ptr CChar, CInt)] -> IO ()
c_prepareIovec _ [] = return ()
c_prepareIovec piov ((p, n) : pns) = do
	#{poke struct iovec, iov_base} piov p
	#{poke struct iovec, iov_len} piov n
	c_prepareIovec (piov `plusPtr` #{size struct iovec}) pns
