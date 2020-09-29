{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.AsPointer (AsPointer(..)) where

import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable, peek, poke)

class AsPointer a where
	asPointer :: a -> (Ptr a -> IO b) -> IO b; asValue :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => AsPointer a where
	asPointer x f = alloca $ (>>) <$> (`poke` x) <*> f; asValue p = peek p
