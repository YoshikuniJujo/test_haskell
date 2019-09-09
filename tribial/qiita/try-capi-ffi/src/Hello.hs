module Hello where

import Foreign.C.Types

foreign import ccall "add123" c_add123 :: CInt -> CInt

foreign import ccall "return_eight" c_eight :: CInt

foreign import ccall "return_ADD123_n" c_ADD123 :: CInt -> CInt
