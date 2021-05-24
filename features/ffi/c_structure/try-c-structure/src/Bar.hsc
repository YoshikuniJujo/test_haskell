{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Bar where

import Foreign.Storable
import Foreign.C.Types

import Foreign.C.Struct

#include "bar.h"

struct "Bar" #{size Bar}
	[("x", ''CInt, [| #{peek Bar, x} |], [| #{poke Bar, x} |])]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded]
