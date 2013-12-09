import Foreign.C.Types

foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
foreign import ccall "math.h log" c_log :: CDouble -> CDouble

e :: Floating a => a
e = 2.718281828459045
