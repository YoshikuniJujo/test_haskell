{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (init)

import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C.Types
import Data.Foldable
import Data.Word
import Data.JuicyCairo
import Data.CairoImage.Internal
import Numeric
import Codec.Picture
import Graphics.Rendering.OpenGL

import Lib
import TryFbo
import GetTextureImage

fboWidth, fboHeight :: GLsizei
fboWidth = 64
fboHeight = 32

main :: IO ()
main = do
	dpy <- xOpenDisplay Nothing
	argb <- makeTriangle dpy fboWidth fboHeight
	xCloseDisplay dpy

	writePngArgb32 "foo.png" argb
	asciiArgb32 argb

render :: Display -> GLsizei -> GLsizei -> IO () -> IO Argb32
render dpy w h dr = glXChooseVisualWith dpy (xDefaultScreen dpy) defaultGlxAttributes {
	glxRgba = True, glxDoublebuffer = True,
	glxRedSize = 1, glxGreenSize = 1, glxBlueSize = 1, glxDepthSize = 1 } \v -> do
	print v
	ctx <- glXCreateContext dpy v Nothing True
	print ctx
	True <- glXMakeCurrent dpy Nothing (Just ctx)
	(fb, cb) <- init w h
	viewport $= (Position 0 0, Size w h)
	bindFramebuffer Framebuffer $= fb

	dr

	flush

	bindFramebuffer Framebuffer $= defaultFramebufferObject

	p <- mallocBytes (fromIntegral $ w * h * 4)

	getTextureImage cb 0 glBgra glUnsignedInt_8_8_8_8_rev
		(w * h * 4) p
	iss <- groups (fromIntegral w)
		<$> peekArray (fromIntegral $ w * h) (castPtr p)
	print $ ((flip (showHex @Word32) "") <$>) <$> iss
	argb <- makeArgb32 (fromIntegral w) (fromIntegral h) p

	True <- glXMakeCurrent dpy Nothing Nothing
	glXDestroyContext dpy ctx

	pure argb

makeTriangle :: Display -> GLsizei -> GLsizei -> IO Argb32
makeTriangle dpy w h =  render dpy w h do
	clearColor $= Color4 0.8 0.4 0.05 1.0
	clear [ColorBuffer]
	color (Color3 1 1 1 :: Color3 GLdouble)
	renderPrimitive LineLoop $ mapM_ vertex [
		Vertex2 (- 0.9) (- 0.9) :: Vertex2 GLdouble,
		Vertex2 (- 0.9) 0.9, Vertex2 0.9 0.9 ]

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n xs = take n xs : groups n (drop n xs)

makeArgb32 :: CInt -> CInt -> Ptr Word32 -> IO Argb32
makeArgb32 w h p = getArgb32 <$> do
	fp <- newForeignPtr p' $ free p'
	pure $ CairoImage CairoFormatArgb32 w h (w * 4) fp
	where
	p' = castPtr p
	getArgb32 = \case CairoImageArgb32 i -> i; _ -> error "not Argb32"
		

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
