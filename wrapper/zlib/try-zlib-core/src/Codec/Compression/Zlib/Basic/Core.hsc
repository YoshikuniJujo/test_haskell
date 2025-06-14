module Codec.Compression.Zlib.Basic.Core where

import Foreign.C.String

foreign import ccall "zlibVersion" c_zlibVersion :: CString
