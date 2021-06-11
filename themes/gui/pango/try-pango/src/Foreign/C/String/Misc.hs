{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.String.Misc (toCStringLen, copyCString) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

toCStringLen :: CString -> IO CStringLen
toCStringLen cs = (cs ,) <$> cStringLength cs

cStringLength :: CString -> IO Int
cStringLength p = peek p >>= \case
	0 -> pure 0
	_ -> (+ 1) <$> cStringLength (p `plusPtr` 1)

copyCString :: CString -> Int -> IO CString
copyCString cs n = do
	cs' <- mallocBytes n
	cs' <$ copyBytes cs' cs n
