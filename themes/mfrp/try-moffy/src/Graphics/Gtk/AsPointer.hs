{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.AsPointer (AsPointer(..)) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

class AsPointer a where
	asPointer :: a -> (Ptr a -> IO b) -> IO b
	asValue :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => AsPointer a where
	asPointer x f = alloca \p -> do
		poke p x
		f p
	asValue p = peek p
