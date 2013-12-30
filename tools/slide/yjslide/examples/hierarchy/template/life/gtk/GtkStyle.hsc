{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

#include <gtk/gtk.h>

module GtkStyle (
	GtkStyle(..),
	SomeGtkStyle(..),

	GtkStateType,
	fgGC,
) where

import Foreign.Ptr
import Foreign.Storable
import Control.Applicative

import GObject
import GdkGC

gClass "GObject" "GtkStyle"

c_fgGC :: Ptr GtkStyle -> Int -> IO (Ptr SomeGdkGC)
c_fgGC gs i = peekElemOff (#{ptr GtkStyle, fg_gc} gs) i

fgGC :: GtkStyle -> GtkStateType -> IO SomeGdkGC
fgGC gs st = SomeGdkGC <$> c_fgGC (pointer gs) (fromEnum st)

data GtkStateType
	= GtkStateNormal
	| GtkStateActive
	| GtkStatePrelight
	| GtkStateSelected
	| GtkStateInsensitive
	deriving (Show, Enum)
