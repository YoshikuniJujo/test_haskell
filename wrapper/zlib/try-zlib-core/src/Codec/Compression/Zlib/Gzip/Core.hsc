{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Gzip.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Data.Int

#include <zlib.h>

data GzFileTag

type GzFile = Ptr GzFileTag

enum "ZError" ''#{type int} [''Show, ''Read, ''Eq] [
	("ZOk", #{const Z_OK}),
	("ZStreamError", #{const Z_STREAM_ERROR}),
	("ZErrno", #{const Z_ERRNO}),
	("ZMemError", #{const Z_MEM_ERROR}),
	("ZBufError", #{const Z_BUF_ERROR}) ]

foreign import ccall "gzopen" c_gzopen :: CString -> CString -> IO GzFile

foreign import ccall "gzgets" c_gzgets ::
	GzFile -> CString -> #{type int} -> IO CString

foreign import ccall "gzclose" c_gzclose :: GzFile -> IO ZError
