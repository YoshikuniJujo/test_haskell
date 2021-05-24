{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FooIx where

import Foreign.Storable (Storable, peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..))
import Foreign.C.Struct (struct)
import Data.Array (Ix(..))

#include "foo.h"

---------------------------------------------------------------------------

-- * DEFINITION
-- * SAMPLE
-- * INSTANCE IX CINT

---------------------------------------------------------------------------
-- DEFINITION
---------------------------------------------------------------------------

newtype CIntIx = CIntIx CInt
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Storable)

struct "FooIx" #{size Foo}
	[	("x", ''CIntIx, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CIntIx, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Eq, ''Ord, ''Ix]

---------------------------------------------------------------------------
-- INSTANCE IX CINTIX
---------------------------------------------------------------------------

instance Ix CIntIx where
	range (l, u) = [l .. u]
	index (l, _) i = fromIntegral $ i - l
	inRange (l, u) i = l <= i && i <= u
