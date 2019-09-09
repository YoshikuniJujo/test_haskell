{-# LANGUAGE CApiFFI #-}

module HelloCApi where

import Foreign.Ptr
import Foreign.C.Types

foreign import capi "hello.h value eight" c_eight :: CInt
foreign import capi "hello.h ADD123" c_ADD123 :: CInt -> CInt

data {-# CTYPE "hello.h" "struct point" #-} Point

foreign import ccall "make_point" c_make_point :: CInt -> CInt -> IO (Ptr Point)
foreign import capi "hello.h POINT_X" c_POINT_X :: Ptr Point -> IO CInt
