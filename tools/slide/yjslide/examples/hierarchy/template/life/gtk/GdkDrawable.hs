{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GdkDrawable (
	SomeGdkWindow(..),
	GdkWindow(..)
) where

import GObject

gClass "GObject" "GdkDrawable"
gClass "GdkDrawable" "GdkWindow"
