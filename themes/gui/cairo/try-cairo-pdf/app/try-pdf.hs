{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Page
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.TagsAndLinks
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Surfaces.PdfSurfaces

main :: IO ()
main = cairoPdfSurfaceWith "try-pdf.pdf" 595 842 \sr -> do
	cairoPdfSurfaceSetMetadata sr CairoPdfMetadataTitle "FOO"
	cairoPdfSurfaceSetMetadata sr CairoPdfMetadataAuthor "Yosh"
	cairoPdfSurfaceSetMetadata sr CairoPdfMetadataSubject "PDF sample"
	cairoPdfSurfaceSetMetadata sr CairoPdfMetadataKeywords "foo, bar, baz"
	cairoPdfSurfaceSetMetadata sr CairoPdfMetadataCreator "Human"
	cairoPdfSurfaceSetMetadata sr CairoPdfMetadataCreateDate "2021-04-09T14:50:50Z"
	cairoPdfSurfaceSetMetadata sr CairoPdfMetadataModDate "1980-02-26T09:15:15Z"

	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.2 0.1
	cairoRectangle cr 50 50 50 50
	cairoTagLinkInternal cr "here" $ cairoFill cr
	cairoFill cr
	cairoRectangle cr 400 50 50 50
	cairoFill cr

	cairoCopyPage cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.2 0.1
	cairoRectangle cr 198.33 280.67 198.33 280.67
	cairoTagLinkUri cr "https://google.com" $ cairoFill cr

	cairoShowPage cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.2 0.5
	cairoRectangle cr 198.33 280.67 198.33 280.67
	cairoFill cr
	cairoRectangle cr 400 600 50 50
	cairoTagDestination cr "here" $ cairoFill cr

	foo <- cairoPdfSurfaceAddOutline sr CairoPdfOutlineRoot "foo" (Left "here")
		[CairoPdfOutlineFlagOpen]
	bar <- cairoPdfSurfaceAddOutline sr foo "bar" (Right (2, Just (50, 500)))
		[CairoPdfOutlineFlagBold]
	baz <- cairoPdfSurfaceAddOutline sr bar "baz" (Right (3, Just (10, 20)))
		[CairoPdfOutlineFlagItalic]
	void $ cairoPdfSurfaceAddOutline sr baz "hoge" (Right (1, Nothing))
		[CairoPdfOutlineFlagOpen]
