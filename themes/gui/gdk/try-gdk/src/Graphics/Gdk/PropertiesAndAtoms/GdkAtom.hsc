{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.PropertiesAndAtoms.GdkAtom where

import Foreign.Ptr
import Foreign.C.String
import Data.Int

#include <gdk/gdk.h>

newtype GdkAtom = GdkAtom (Ptr GdkAtom) deriving Show

gdkAtomIntern :: String -> IO GdkAtom
gdkAtomIntern nm = (GdkAtom <$>)
	$ withCString nm \cnm -> c_gdk_atom_intern cnm #{const FALSE}

foreign import ccall "gdk_atom_intern" c_gdk_atom_intern ::
	CString -> #{type gboolean} -> IO (Ptr GdkAtom)
