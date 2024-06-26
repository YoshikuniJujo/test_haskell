{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ptr where

import Foreign.Ptr

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> True)
	where NullPtr = nullPtr
