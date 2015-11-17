import Foreign
import Foreign.C.Types

foreign import ccall unsafe "math.h sin" c_sin :: CDouble -> CDouble

hsSin :: Double -> Double
hsSin = realToFrac . c_sin . realToFrac
