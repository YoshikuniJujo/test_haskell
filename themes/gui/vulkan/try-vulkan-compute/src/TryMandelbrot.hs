{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMandelbrot where

import Foreign.C.Types
import Data.Complex

foreign import ccall "escape_time_hs" c_escape_time_hs ::
	CFloat -> CFloat -> CInt -> CInt

escapeTime :: Complex CFloat -> CInt -> Maybe CInt
escapeTime (re :+ im) lm = case c_escape_time_hs re im lm of
	t | t < 0 -> Nothing
	t -> Just t
