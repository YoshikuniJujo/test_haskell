{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GtkButton (
) where

import Foreign.Ptr

import GtkBin

data GtkButtonPtr
class GtkButton b where
	gtkButtonPtr :: b -> Ptr GtkButtonPtr
instance GtkButton b => GtkBin b where
	gtkBinPtr = castPtr . gtkButtonPtr
