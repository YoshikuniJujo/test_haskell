{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Simple where

import Control.Monad.Primitive
import Data.Vector
import Data.Vector.Generic.Mutable qualified as V
import Data.Word
import Data.Color qualified as Color

data I s = I { width :: Int, height :: Int, body :: MVector s Word8 }

new :: PrimMonad m => Int -> Int -> m (I (PrimState m))
new w h = I w h <$> V.new (w * h * 4)

write :: (PrimMonad m, RealFrac d) =>
	I (PrimState m) -> Int -> Int -> Color.Rgba d -> m ()
write I { width = w, height = _h, body = bd } x y (Color.RgbaWord8 r g b a) = do
	V.write bd i0 r; V.write bd (i0 + 1) g;
	V.write bd (i0 + 2) b; V.write bd (i0 + 3) a
	where i0 = 4 * (w * y + x)

read :: (PrimMonad m, RealFrac d) =>
	I (PrimState m) -> Int -> Int -> m (Color.Rgba d)
read I { width = w, height = _h, body = bd } x y = Color.RgbaWord8
	<$> V.read bd i0
	<*> V.read bd (i0 + 1)
	<*> V.read bd (i0 + 2)
	<*> V.read bd (i0 + 3)
	where i0 = 4 * (w * y + x)

data Gray s = Gray {
	grayWidth :: Int, grayHeight :: Int, grayBody :: MVector s Word8 }

grayNew :: PrimMonad m => Int -> Int -> m (Gray (PrimState m))
grayNew w h = Gray w h <$> V.new (w * h)

grayWrite :: PrimMonad m =>
	Gray (PrimState m) -> Int -> Int -> Word8 -> m ()
grayWrite Gray { grayWidth = w, grayHeight = _h, grayBody = bd } x y g =
	V.write bd (w * y + x) g

grayRead :: PrimMonad m => Gray (PrimState m) -> Int -> Int -> m Word8
grayRead Gray { grayWidth = w, grayHeight = _h, grayBody = bd } x y =
	V.read bd $ w * y + x
