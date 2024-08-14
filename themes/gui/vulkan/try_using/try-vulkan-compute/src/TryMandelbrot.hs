{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMandelbrot (render) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Data.Complex
import Data.Word

render :: (Word32, Word32) -> Complex Float -> Complex Float -> IO [Word32]
render (w, h)
	((realToFrac -> l) :+ (realToFrac -> t))
	((realToFrac -> r) :+ (realToFrac -> b)) =
	allocaArray (fromIntegral $ w * h) \p ->
		c_render p w h l t r b >> peekArray (fromIntegral $ w * h) p


foreign import ccall "render" c_render ::
	Ptr Word32 ->
	Word32 -> Word32 -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
