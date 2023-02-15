{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module UseTryClass where

import TryClass

instance Foo Int where
	bar = id
