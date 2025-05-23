{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGe RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pipe.Draw (drawCairoImageRgba32) where

import Foreign.C.Types
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.List qualified as L
import Data.Word
import Data.ByteString qualified as BS
import Data.CairoImage

drawCairoImageRgba32 :: forall m ->
	(PrimMonad m, U.Member Pipe.P es, U.Base (U.FromFirst m) es) =>
	Argb32Mut (PrimState m) -> CInt -> CInt -> Eff.E es BS.ByteString o ()
drawCairoImageRgba32 m img w h = ($ 0) $ fix \go p ->
	if p < w * h
	then do
		cs <- byteStringToTuple4s <$> Pipe.await
		let	ln = L.genericLength cs
		Eff.effBase $ for_ ([p .. p + ln - 1] `zip` cs) \(p', c) ->
			uncurry4 (draw p') $ rgbaToArgb c
		go $ p + ln
	else pure ()
	where
	draw :: CInt -> Word8 -> Word8 -> Word8 -> Word8 -> m ()
	draw p a r g b = putPixel (img :: Argb32Mut (PrimState m)) (p `mod` w) (p `div` w)
		(PixelArgb32Straight a r g b)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, w) = f x y z w

rgbaToArgb :: (r, g, b, a) -> (a, r, g, b)
rgbaToArgb (r, g, b, a) = (a, r, g, b)

byteStringToTuple4s :: BS.ByteString -> [(Word8, Word8, Word8, Word8)]
byteStringToTuple4s = go . BS.unpack
	where
	go (x : y : z : w : xs) = (x, y, z, w) : go xs
	go _ = []
