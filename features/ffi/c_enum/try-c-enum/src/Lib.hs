{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Storable
import Foreign.C.Enum

enum "Foo" ''Int [''Show, ''Read, ''Storable] [
	("FooBar", 123),
	("FooBaz", 456) ]
