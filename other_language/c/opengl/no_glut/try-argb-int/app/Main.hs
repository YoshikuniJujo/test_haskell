{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (init)

import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal
import Data.Foldable
import Data.Word
import Data.JuicyCairo
import Data.CairoImage.Internal
import Codec.Picture
import Graphics.Rendering.OpenGL

import Lib
import GetTextureImage

main :: IO ()
main = do
	argb <- render (Size 64 32) do
		clearColor $= Color4 0.8 0.4 0.05 1.0
		clear [ColorBuffer]
		color (Color3 1 1 1 :: Color3 GLdouble)
		renderPrimitive LineLoop $ mapM_ vertex [
			Vertex2 (- 0.9) (- 0.9) :: Vertex2 GLdouble,
			Vertex2 (- 0.9) 0.9, Vertex2 0.9 0.9 ]

	writePngArgb32 "foo.png" argb
	asciiArgb32 argb

writePngArgb32 :: FilePath -> Argb32 -> IO ()
writePngArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8

asciiArgb32 :: Argb32 -> IO ()
asciiArgb32 argb = do
	let	CairoImage _ w h s fd = CairoImageArgb32 argb
	bsss <- withForeignPtr fd \pd -> groups (fromIntegral w) . groups 4
		<$> peekArray (fromIntegral $ s * h) (castPtr pd)
	print @[[[Word8]]] bsss
	for_ bsss \bss -> do
		for_ bss \cs -> do
			let	[b, g, r, _a] = fromIntegral <$> cs
				x :: Int = b + g + r
			case () of
				_	| x <= 0xbf  -> putStr " "
					| x <= 0x17e -> putStr "."
					| x <= 0x23d -> putStr "+"
					| x <= 0x23d -> putStr "*"
					| otherwise -> putStr "#"
		putStrLn ""
