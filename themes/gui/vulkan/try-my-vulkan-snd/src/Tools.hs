{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String

pokeCString :: CString -> String -> IO ()
pokeCString cs str = withCStringLen str \(cs_, ln) -> do
	copyBytes cs cs_ ln
	poke (cs `plusPtr` ln :: CString) 0
