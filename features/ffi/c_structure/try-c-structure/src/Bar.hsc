{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Bar where

import Foreign.Storable
import Foreign.C.Types
import Data.Array

import Template

#include "foo.h"

struct "Bar" #{size Bar}
	[("x", ''CInt, [| #{peek Bar, x} |], [| #{poke Bar, x} |])]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded, ''Ix]

---------------------------------------------------------------------------
-- INSTANCE IX CINT
---------------------------------------------------------------------------

instance Ix CInt where
	range (l, u) = [l .. u]
	index (l, _) i = fromIntegral $ i - l
	inRange (l, u) i = l <= i && i <= u
