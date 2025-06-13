{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Error.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Data.Int

#include <zlib.h>

enum "E" ''#{type int} [''Show, ''Read, ''Eq] [
	("ZOk", #{const Z_OK}),
	("ZStreamError", #{const Z_STREAM_ERROR}),
	("ZErrno", #{const Z_ERRNO}),
	("ZMemError", #{const Z_MEM_ERROR}),
	("ZBufError", #{const Z_BUF_ERROR}),
	("ZDataError", #{const Z_DATA_ERROR})
	]
