{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "math.h sin" c_sin :: CDouble -> CDouble

haskellSin :: Double -> Double
haskellSin = realToFrac . c_sin . realToFrac
