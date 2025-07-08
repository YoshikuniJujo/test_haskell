{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Simple where

import Control.Monad.Primitive
import Data.Vector
import Data.Vector.Generic.Mutable qualified as V
import Data.Word
import Data.Color

data I s = I { width :: Int, height :: Int, body :: MVector s Word8 }

new :: PrimMonad m => Int -> Int -> m (I (PrimState m))
new w h = I w h <$> V.new (w * h * 4)

write :: (PrimMonad m, RealFrac d) =>
	I (PrimState m) -> Int -> Int -> Rgba d -> m ()
write I { width = w, height = _h, body = bd } x y (RgbaWord8 r g b a) = do
	V.write bd i0 r; V.write bd (i0 + 1) g;
	V.write bd (i0 + 2) b; V.write bd (i0 + 3) a
	where i0 = 4 * (w * y + x)
