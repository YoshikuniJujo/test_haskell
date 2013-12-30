{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GdkGC (
	GdkGC(..),
	SomeGdkGC(..)
) where

import GObject

gClass "GObject" "GdkGC"
