{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Gzip.Core (

	GzFile,
	c_gzopen, c_gzgets, c_gzclose

) where

import Foreign.Ptr
import Foreign.C.String
import Data.Int

import Codec.Compression.Zlib.Constant.Core

#include <zlib.h>

data GzFileTag

type GzFile = Ptr GzFileTag

foreign import ccall "gzopen" c_gzopen :: CString -> CString -> IO GzFile

foreign import ccall "gzgets" c_gzgets ::
	GzFile -> CString -> #{type int} -> IO CString

foreign import ccall "gzclose" c_gzclose :: GzFile -> IO ReturnCode
