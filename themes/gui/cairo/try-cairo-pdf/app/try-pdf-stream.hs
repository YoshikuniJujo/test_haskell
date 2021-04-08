{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Control.Monad.ST
import Data.STRef
import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.PdfSurfaces
import Graphics.Cairo.Surfaces.CairoWriteFuncT

import qualified Data.ByteString as BS

main :: IO ()
main = do
{-
	cairoPdfSurfaceWithForStream (\_ t -> T.putStrLn t >> pure WriteSuccess) nullPtr 128 128 \sr -> do
	--	sr <- cairoSvgSurfaceCreateForStream (\_ t -> T.putStrLn t >> pure WriteSuccess) nullPtr 128 128
		cr <- cairoCreate sr
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
		cairoPaint cr
	--	cairoSurfaceFlush sr
	--	cairoSurfaceFinish sr
-}
	putStrLn "=== ST ==="
	BS.writeFile "try-pdf-stream-st.pdf" $ runST st

st :: ST s BS.ByteString
st = newSTRef "" >>= \v -> do
	cairoPdfSurfaceWithForStream (\_ t -> modifySTRef v (<> t) >> pure WriteSuccess) nullPtr 128 128 \sr -> do
	--	sr <- cairoSvgSurfaceCreateForStream (\_ t -> modifySTRef v (<> t) >> pure WriteSuccess) nullPtr 128 128
		cr <- cairoCreate sr
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
		cairoPaint cr
	--	cairoSurfaceFinish sr
	readSTRef v
