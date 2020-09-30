{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Tools where

import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent

withPtrForeignPtr :: PtrForeignPtr a -> (Ptr a -> IO b) -> IO b
withPtrForeignPtr (Left p) f = f p
withPtrForeignPtr (Right fp) f = withForeignPtr fp f

type PtrForeignPtr a = Either (Ptr a) (ForeignPtr a)

wrapPtr :: Ptr a -> PtrForeignPtr a
wrapPtr = Left

setFinalizer :: Ptr a -> IO () -> IO (PtrForeignPtr a)
setFinalizer p fnl = Right <$> newForeignPtr p fnl
