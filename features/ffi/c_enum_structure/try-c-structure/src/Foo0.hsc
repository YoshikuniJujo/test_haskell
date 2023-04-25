{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo0 where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.C.Struct (struct)

#include "foo.h"

struct "Foo0" #{size Foo} #{alignment Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CUInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded]

fooPrint :: Foo0 -> IO ()
fooPrint (Foo0_ f) = withForeignPtr f c_foo_print

foreign import ccall "foo_print" c_foo_print :: Ptr Foo0 -> IO ()
