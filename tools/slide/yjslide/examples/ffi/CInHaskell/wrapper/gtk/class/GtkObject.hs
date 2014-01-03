{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GtkObject (
	GtkObject(..)
) where

import Foreign.Ptr
import GObject

data GtkObjectPtr
class GtkObject o where
	gtkObjectPtr :: o -> Ptr GtkObjectPtr
instance GtkObject o => GObject o where
	gObjectPtr = castPtr . gtkObjectPtr
