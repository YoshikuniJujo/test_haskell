module Foldl() where

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "foldl.h foldl" c_foldl ::
	FunPtr (CInt -> CInt -> CInt) -> CInt -> CInt -> Ptr CInt -> CInt

foreign import ccall unsafe "foldl.h &add" c_add :: FunPtr (CInt -> CInt -> CInt)
foreign import ccall unsafe "foldl.h &mul" c_mul :: FunPtr (CInt -> CInt -> CInt)

foreign import ccall unsafe "foldl.h &sample" c_sample :: Ptr CInt

foreign import ccall unsafe "wrapper" wrap ::
	(CInt -> CInt -> CInt) -> IO (FunPtr (CInt -> CInt -> CInt))
