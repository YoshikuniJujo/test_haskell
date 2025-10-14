{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Simple where

import Control.Monad.Primitive
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable qualified as VG
import Data.Word
import Data.Color qualified as Color

import Data.Image.Gray qualified as Immutable

data I s = I { width :: Int, height :: Int, body :: V.MVector s Word8 }

new :: PrimMonad m => Int -> Int -> m (I (PrimState m))
new w h = I w h <$> VG.new (w * h * 4)

write :: (PrimMonad m, RealFrac d) =>
	I (PrimState m) -> Int -> Int -> Color.Rgba d -> m ()
write I { width = w, height = _h, body = bd } x y (Color.RgbaWord8 r g b a) = do
	VG.write bd i0 r; VG.write bd (i0 + 1) g;
	VG.write bd (i0 + 2) b; VG.write bd (i0 + 3) a
	where i0 = 4 * (w * y + x)

read :: (PrimMonad m, RealFrac d) =>
	I (PrimState m) -> Int -> Int -> m (Color.Rgba d)
read I { width = w, height = _h, body = bd } x y = Color.RgbaWord8
	<$> VG.read bd i0
	<*> VG.read bd (i0 + 1)
	<*> VG.read bd (i0 + 2)
	<*> VG.read bd (i0 + 3)
	where i0 = 4 * (w * y + x)

data Gray s = Gray {
	grayWidth :: Int, grayHeight :: Int, grayBody :: V.MVector s Word8 }

grayNew :: PrimMonad m => Int -> Int -> m (Gray (PrimState m))
grayNew w h = Gray w h <$> VG.new (w * h)

grayWrite :: PrimMonad m =>
	Gray (PrimState m) -> Int -> Int -> Word8 -> m ()
grayWrite Gray { grayWidth = w, grayHeight = _h, grayBody = bd } x y g =
	VG.write bd (w * y + x) g

grayRead :: PrimMonad m => Gray (PrimState m) -> Int -> Int -> m Word8
grayRead Gray { grayWidth = w, grayHeight = _h, grayBody = bd } x y =
	VG.read bd $ w * y + x

grayFreeze :: forall m . PrimMonad m => Gray (PrimState m) -> m Immutable.G
grayFreeze Gray { grayWidth = w, grayHeight = h, grayBody = bd } = do
	bd' <- V.freeze @m bd
	pure Immutable.G { Immutable.grayWidth = w, Immutable.grayHeight = h, Immutable.grayBody = bd' }
