{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.String.ForeignCStringLen (
	ForeignCStringLen, copyToForeignCStringLen, withForeignCStringLen
	) where

import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types

type ForeignCStringLen = (ForeignPtr CChar, Int)

copyToForeignCStringLen :: CStringLen -> IO ForeignCStringLen
copyToForeignCStringLen (cs, n) = do
	p <- mallocBytes n
	copyBytes p cs n
	newForeignCStringLen (p, n) (free p)

newForeignCStringLen :: CStringLen -> IO () -> IO ForeignCStringLen
newForeignCStringLen (p, l) fr = (, l) <$> newForeignPtr p fr

withForeignCStringLen :: ForeignCStringLen -> (CStringLen -> IO a) -> IO a
withForeignCStringLen (fp, l) f = withForeignPtr fp $ f . (, l)
