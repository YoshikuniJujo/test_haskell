module Lib where

import Foreign.C.Types

#include <foo.h>

foo :: CInt
foo = #{const FOO}

foreign import ccall "add" c_add :: CInt -> CInt -> CInt
