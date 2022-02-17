{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String
import Control.Monad.Cont

cStringListToCStringArray :: [CString] -> ContT r IO (Ptr CString)
cStringListToCStringArray cstrs = do
	pcstrs <- ContT $ allocaArray cstrc
	lift $ pokeArray pcstrs cstrs
	pure pcstrs
	where cstrc = length cstrs

pokeCString :: CString -> String -> IO ()
pokeCString cs str = withCStringLen str \(cs_, ln) -> do
	copyBytes cs cs_ ln
	poke (cs `plusPtr` ln :: CString) 0
