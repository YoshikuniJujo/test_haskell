{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMandelbrot where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.Types
import Data.Complex
import System.IO.Unsafe

foreign import ccall "escape_time_hs" c_escape_time_hs ::
	CFloat -> CFloat -> CInt -> CInt

escapeTime :: Complex CFloat -> CInt -> Maybe CInt
escapeTime (re :+ im) lm = case c_escape_time_hs re im lm of
	t | t < 0 -> Nothing
	t -> Just t

foreign import ccall "pixel_to_point_hs" c_pixel_to_point_hs ::
	CInt -> CInt -> CInt -> CInt -> CFloat -> CFloat -> CFloat -> CFloat ->
	Ptr CFloat -> Ptr CFloat -> IO ()

pixelToPoint :: (CInt, CInt) -> (CInt, CInt) ->
	Complex CFloat -> Complex CFloat -> Complex CFloat
pixelToPoint (w, h) (x, y) (lft :+ upr) (rgt :+ lwr) = unsafePerformIO
	$ alloca \pre -> alloca \pim -> do
		c_pixel_to_point_hs w h x y lft upr rgt lwr pre pim
		(:+) <$> peek pre <*> peek pim
