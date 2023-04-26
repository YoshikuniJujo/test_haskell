{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pnext.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

#include "foo.h"

enum "StructureType" ''#{type StructureType}
		[''Show, ''Read, ''Eq, ''Storable, ''Ord] [
	("StructureTypeInt", #{const STRUCTURE_TYPE_INT}),
	("StructureTypeFloat", #{const STRUCTURE_TYPE_FLOAT}),
	("StructureTypeDouble", #{const STRUCTURE_TYPE_DOUBLE}) ]
