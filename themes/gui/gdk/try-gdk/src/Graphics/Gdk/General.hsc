{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.General where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C

#include <gdk/gdk.h>

foreign import ccall "gdk_init" c_gdk_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

gdkInit :: [String] -> IO [String]
gdkInit as = allocaArray (length as) \arr -> do
	cas <- newCString `mapM` as
	pokeArray arr cas
	(n', arr') <- alloca \pn -> do
		poke pn . fromIntegral $ length as
		arr' <- alloca \parr -> do
			poke parr arr
			c_gdk_init pn parr
			peek parr
		(, arr') <$> peek pn
	(peekCString `mapM`) =<< peekArray (fromIntegral n') arr'
