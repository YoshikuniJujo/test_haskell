module Pi (pi) where

import Prelude hiding (pi)
import Foreign.C.Types

foreign export ccall "h_pi" pi :: CUInt -> CDouble

pi :: CUInt -> CDouble
pi n = 4 * getPi4 0 1 n

getPi4 :: CDouble -> CDouble -> CUInt -> CDouble
getPi4 p _ 0 = p
getPi4 p i n = let
	p' = p + recip i
	i' = negate $ i + 2 * signum i in
	p' `seq` i' `seq` getPi4 p' i' (n - 1)
