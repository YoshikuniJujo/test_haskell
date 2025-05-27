{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGe RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pipe.Draw (
	drawCairoImageRgba32, drawCairoImageRgba32Adam7,
	drawCairoImageRgb24, drawCairoImageRgb24Adam7 ) where

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
	(PrimMonad m, U.Member Pipe.P es, U.Base (U.FromFirst m) es) => Bool ->
	Argb32Mut (PrimState m) -> CInt -> CInt -> Eff.E es BS.ByteString o () -> Eff.E es BS.ByteString o ()
drawCairoImageRgba32Adam7 m blk img w h act = ($ 0) $ fix \go p ->
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
	draw p a r g b = if blk
		then uncurry (putBlock img) xywh (PixelArgb32Straight a r g b)
		else putPixel (img :: Argb32Mut (PrimState m)) x y
			(PixelArgb32Straight a r g b)
		where xywh@((x, y), _) = calcPos w h p

putBlock :: (PrimMonad m, ImageMut im) =>
	im (PrimState m) -> (CInt, CInt) -> (CInt, CInt) -> PixelMut im -> m ()
putBlock img (x0, y0) (w, h) px =
	for_ [x0 .. x0 + w - 1] \x -> for_ [y0 .. y0 + h - 1] \y -> putPixel img x y px

calcPos :: Integral n => n -> n -> n -> ((n, n), (n, n))
calcPos w h p
	| p < wh1 = ((p `mod` w1 * 8, p `div` w1 * 8), (8, 8))
	| p < wh2 = let p' = p - wh1 in ((p' `mod` w2 * 8 + 4, p' `div` w2 * 8), (4, 8))
	| p < wh3 = let p' = p - wh2 in ((p' `mod` w3 * 4, p' `div` w3 * 8 + 4), (4, 4))
	| p < wh4 = let p' = p - wh3 in ((p' `mod` w4 * 4 + 2, p' `div` w4 * 4), (2, 4))
	| p < wh5 = let p' = p - wh4 in ((p' `mod` w5 * 2, p' `div` w5 * 4 + 2), (2, 2))
	| p < wh6 = let p' = p - wh5 in ((p' `mod` w6 * 2 + 1, p' `div` w6 * 2), (1, 2))
	| p < wh7 = let p' = p - wh6 in ((p' `mod` w, p' `div` w * 2 + 1), (1, 1))
	where
	wh1 = (w `div'` 8) * (h `div'` 8)
	wh2 = (w `div'` 4) * (h `div'` 8)
	wh3 = (w `div'` 4) * (h `div'` 4)
	wh4 = (w `div'` 2) * (h `div'` 4)
	wh5 = (w `div'` 2) * (h `div'` 2)
	wh6 = w * (h `div'` 2)
	wh7 = w * h
	w1 = w `div'` 8
	w2 = w `div'` 4 `div` 2
	w3 = w `div'` 4
	w4 = w `div'` 2 `div` 2
	w5 = w `div'` 2
	w6 = w `div` 2

a `div'` b = (a - 1) `div` b + 1

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

drawCairoImageRgb24Adam7 :: forall m ->
	(PrimMonad m, U.Member Pipe.P es, U.Base (U.FromFirst m) es) => Bool ->
	Argb32Mut (PrimState m) -> CInt -> CInt -> Eff.E es BS.ByteString o () ->
	Eff.E es BS.ByteString o ()
drawCairoImageRgb24Adam7 m blk img w h act = ($ 0) $ fix \go p ->
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
	draw p a r g b = if blk
		then uncurry (putBlock img) xywh (PixelArgb32Straight a r g b)
		else putPixel (img :: Argb32Mut (PrimState m)) x y
			(PixelArgb32Straight a r g b)
		where xywh@((x, y), _) = calcPos w h p

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
