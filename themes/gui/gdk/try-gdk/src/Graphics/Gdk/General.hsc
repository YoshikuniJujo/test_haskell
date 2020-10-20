{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.General (
	gdkInit
	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Int

#include <gdk/gdk.h>

foreign import ccall "gdk_init" c_gdk_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall "gdk_init_check" c_gdk_init_check :: Ptr CInt -> Ptr (Ptr CString) -> IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool = \case #{const FALSE} -> False; _ -> True

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
