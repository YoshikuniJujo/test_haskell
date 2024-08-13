{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMandelbrot where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Data.Complex
import System.IO.Unsafe

import Data.Vector qualified as V
import Data.Word

foreign import ccall "escape_time_hs" c_escape_time_hs ::
	CFloat -> CFloat -> CInt -> CInt

escapeTime :: Complex CFloat -> CInt -> Maybe CInt
escapeTime (re :+ im) lm = case c_escape_time_hs re im lm of
	t | t < 0 -> Nothing
	t -> Just t

foreign import ccall "pixel_to_point_hs" c_pixel_to_point_hs ::
	Word32 -> Word32 -> Word32 -> Word32 -> CFloat -> CFloat -> CFloat -> CFloat ->
	Ptr CFloat -> Ptr CFloat -> IO ()

pixelToPoint :: (Word32, Word32) -> (Word32, Word32) ->
	Complex CFloat -> Complex CFloat -> Complex CFloat
pixelToPoint (w, h) (x, y) (lft :+ upr) (rgt :+ lwr) = unsafePerformIO
	$ alloca \pre -> alloca \pim -> do
		c_pixel_to_point_hs w h x y lft upr rgt lwr pre pim
		(:+) <$> peek pre <*> peek pim

foreign import ccall "render_hs" c_render_hs ::
	Ptr Word32 -> Word32 -> Word32 -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

render :: (Word32, Word32) -> Complex Float -> Complex Float -> IO (V.Vector Word32)
render (w, h) ((realToFrac -> lft) :+ (realToFrac -> upr)) ((realToFrac -> rgt) :+ (realToFrac -> lwr)) =
	allocaArray (fromIntegral $ w * h) \ppxl -> do
		c_render_hs ppxl w h lft upr rgt lwr
		V.fromList <$> peekArray (fromIntegral $ w * h) ppxl
