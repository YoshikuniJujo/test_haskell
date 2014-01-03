{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GtkBin (
	GtkBin(..)
) where

import Foreign.Ptr
import GtkContainer

data GtkBinPtr
class GtkBin b where
	gtkBinPtr :: b -> Ptr GtkBinPtr
instance GtkBin b => GtkContainer b where
	gtkContainerPtr = castPtr . gtkBinPtr
