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

import Codec.Compression.Zlib.Error.Core qualified as Error

#include <zlib.h>

data GzFileTag

type GzFile = Ptr GzFileTag

foreign import ccall "gzopen" c_gzopen :: CString -> CString -> IO GzFile

foreign import ccall "gzgets" c_gzgets ::
	GzFile -> CString -> #{type int} -> IO CString

foreign import ccall "gzclose" c_gzclose :: GzFile -> IO Error.E
