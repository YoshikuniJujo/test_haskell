{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Page
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.TagsAndLinks
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Surfaces.PdfSurfaces

import Language.Haskell.TH
import System.Directory
import Graphics.Cairo.Exception

runIO do
	putStrLn =<< getCurrentDirectory
	pure []

main :: IO ()
main = cairoPdfSurfaceWith "try-link-file.pdf" 595 842 \sr -> do
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.2 0.1
	cairoRectangle cr 50 50 50 50
--	cairoTagLinkFile cr "try-pdf.pdf" (Left "here") $ cairoFill cr
	cairoFill cr
	raiseIfError cr
	cairoFill cr
	cairoRectangle cr 400 50 50 50
	cairoFill cr
	cairoShowPage cr
