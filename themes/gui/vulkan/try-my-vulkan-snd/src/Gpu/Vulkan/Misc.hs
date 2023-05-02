{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Misc (nil, nil', cstrToText, newDefaultIORef) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Data.Default
import Data.Text qualified as Txt
import Data.Text.Foreign qualified as Txt
import Data.IORef

import Data.TypeLevel.ParMaybe qualified as TPMaybe

nil :: Maybe (t ())
nil = Nothing

nil' :: TPMaybe.M t 'Nothing
nil' = TPMaybe.N

cstrToText :: CString -> IO Txt.Text
cstrToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs

newDefaultIORef :: Default a => IO (IORef a)
newDefaultIORef = newIORef def
