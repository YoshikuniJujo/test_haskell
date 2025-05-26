{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGe RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pipe.Draw (
	drawCairoImageRgba32, drawCairoImageRgba32Adam7,
	drawCairoImageRgb24 ) where

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
	Argb32Mut (PrimState m) -> CInt -> CInt -> Eff.E es BS.ByteString o () -> Eff.E es BS.ByteString o ()
drawCairoImageRgba32 m img w h act = ($ 0) $ fix \go p ->
	if p < w * h
	then do
		cs <- byteStringToTuple4s <$> Pipe.await
		let	ln = L.genericLength cs
		Eff.effBase $ for_ ([p .. p + ln - 1] `zip` cs) \(p', c) ->
			uncurry4 (draw p') $ rgbaToArgb c
		act
		go $ p + ln
	else pure ()
	where
	draw :: CInt -> Word8 -> Word8 -> Word8 -> Word8 -> m ()
	draw p a r g b = putPixel (img :: Argb32Mut (PrimState m)) (p `mod` w) (p `div` w)
		(PixelArgb32Straight a r g b)

drawCairoImageRgba32Adam7 :: forall m ->
	(PrimMonad m, U.Member Pipe.P es, U.Base (U.FromFirst m) es) =>
	Argb32Mut (PrimState m) -> CInt -> CInt -> Eff.E es BS.ByteString o () -> Eff.E es BS.ByteString o ()
drawCairoImageRgba32Adam7 m img w h act = ($ 0) $ fix \go p ->
	if p < w * h
	then do
		cs <- byteStringToTuple4s <$> Pipe.await
		let	ln = L.genericLength cs
		Eff.effBase $ for_ ([p .. p + ln - 1] `zip` cs) \(p', c) ->
			uncurry4 (draw p') $ rgbaToArgb c
		act
		go $ p + ln
	else pure ()
	where
	draw :: CInt -> Word8 -> Word8 -> Word8 -> Word8 -> m ()
	draw p a r g b = putPixel (img :: Argb32Mut (PrimState m)) x y
		(PixelArgb32Straight a r g b)
		where (x, y) = calcPos w h p

calcPos :: Integral n => n -> n -> n -> (n, n)
calcPos w h p
	| p < wh64 = (p `mod` w8 * 8, p `div` w8 * 8)
	| p < wh32 = let p' = p - wh64 in (p' `mod` w8 * 8 + 4, p' `div` w8 * 8)
	| p < wh16 = let p' = p - wh32 in (p' `mod` w4 * 4, p' `div` w4 * 8 + 4)
	| p < wh8 = let p' = p - wh16 in (p' `mod` w4 * 4 + 2, p' `div` w4 * 4)
	| p < wh4 = let p' = p - wh8 in (p' `mod` w2 * 2, p' `div` w2 * 4 + 2)
	| p < wh2 = let p' = p - wh4 in (p' `mod` w2 * 2 + 1, p' `div` w2 * 2)
	| p < wh = let p' = p - wh2 in (p' `mod` w, p' `div` w * 2 + 1)
	where
	wh64 = w * h `div` 64
	wh32 = w * h `div` 32
	wh16 = w * h `div` 16
	wh8 = w * h `div` 8
	wh4 = w * h `div` 4
	wh2 = w * h `div` 2
	wh = w * h
	w8 = w `div` 8
	w4 = w `div` 4
	w2 = w `div` 2

drawCairoImageRgb24 :: forall m ->
	(PrimMonad m, U.Member Pipe.P es, U.Base (U.FromFirst m) es) =>
	Argb32Mut (PrimState m) -> CInt -> CInt -> Eff.E es BS.ByteString o () ->
	Eff.E es BS.ByteString o ()
drawCairoImageRgb24 m img w h act = ($ 0) $ fix \go p ->
	if p < w * h
	then do
		cs <- byteStringToTuple3s <$> Pipe.await
		let	ln = L.genericLength cs
		Eff.effBase $ for_ ([p .. p + ln - 1] `zip` cs) \(p', c) ->
			uncurry4 (draw p') $ rgbToArgb c
		act
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

rgbToArgb :: (r, g, b) -> (Word8, r, g, b)
rgbToArgb (r, g, b) = (255, r, g, b)

byteStringToTuple4s :: BS.ByteString -> [(Word8, Word8, Word8, Word8)]
byteStringToTuple4s = go . BS.unpack
	where
	go (x : y : z : w : xs) = (x, y, z, w) : go xs
	go _ = []

byteStringToTuple3s :: BS.ByteString -> [(Word8, Word8, Word8)]
byteStringToTuple3s = go . BS.unpack
	where
	go (x : y : z : xs) = (x, y, z) : go xs
	go _ = []
