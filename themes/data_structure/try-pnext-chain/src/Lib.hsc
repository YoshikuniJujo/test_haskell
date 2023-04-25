{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Struct
import Data.Word
import Data.Int

#include "foo.h"

type PtrVoid = Ptr ()

struct "Foo" #{size Foo} #{alignment Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
	 	("y", ''CInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded, ''Storable]

struct "IntValue" #{size IntValue} #{alignment IntValue} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke IntValue, sType} p
			(#{const STRUCTURE_TYPE_INT} ::
				#{type StructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek IntValue, pNext} |],
		[| #{poke IntValue, pNext} |]),
	("intNum", ''#{type int},
		[| #{peek IntValue, intNum} |],
		[| #{poke IntValue, intNum} |] ) ]
	[''Show, ''Eq, ''Ord, ''Storable]
