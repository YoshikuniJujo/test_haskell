{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pnext.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

#include "foo.h"

type PtrVoid = Ptr ()

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

struct "FloatValue" #{size FloatValue} #{alignment FloatValue} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke FloatValue, sType} p
			(#{const STRUCTURE_TYPE_FLOAT} ::
				#{type StructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek FloatValue, pNext} |],
		[| #{poke FloatValue, pNext} |]),
	("floatNum", ''#{type float},
		[| #{peek FloatValue, floatNum} |],
		[| #{poke FloatValue, floatNum} |] ) ]
	[''Show, ''Eq, ''Ord, ''Storable]

struct "DoubleValue" #{size DoubleValue} #{alignment DoubleValue} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke DoubleValue, sType} p
			(#{const STRUCTURE_TYPE_DOUBLE} ::
				#{type StructureType}) |]),
	("doubleNum", ''#{type double},
		[| #{peek DoubleValue, doubleNum} |],
		[| #{poke DoubleValue, doubleNum} |] ) ]
	[''Show, ''Eq, ''Ord, ''Storable]
