{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iovec (
	withIovec, Iovec, AsCCharPtrLenList,
	PtrLenList(..), HeteroPtrLenList(..) ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.Types
import Control.Arrow
import Data.List

import PtrLenList

withIovec :: AsCCharPtrLenList pl => pl -> (Ptr Iovec -> CInt -> IO a) -> IO a
withIovec pns = c_withIovec $ second fromIntegral <$> toCCharPtrLenList pns

#include <sys/uio.h>

data {-# CTYPE "sys/uio.h" "struct iovec" #-} Iovec

c_withIovec :: [(Ptr CChar, CInt)] -> (Ptr Iovec -> CInt -> IO a) -> IO a
c_withIovec pns act = allocaBytes (ln * #{size struct iovec}) $ \piov -> do
	c_prepareIovec piov pns
	act piov ln
	where
	ln :: Num n => n
	ln = genericLength pns

c_prepareIovec :: Ptr Iovec -> [(Ptr CChar, CInt)] -> IO ()
c_prepareIovec _ [] = return ()
c_prepareIovec piov ((p, n) : pns) = do
	#{poke struct iovec, iov_base} piov p
	#{poke struct iovec, iov_len} piov n
	c_prepareIovec (piov `plusPtr` #{size struct iovec}) pns
