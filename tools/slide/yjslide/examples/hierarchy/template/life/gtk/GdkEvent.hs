{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GdkEvent (
	GdkEvent(..),
	GdkEventButton(..)
) where

import GObject

gClass "GObject" "GdkEvent"
gClass "GdkEvent" "GdkEventButton"
