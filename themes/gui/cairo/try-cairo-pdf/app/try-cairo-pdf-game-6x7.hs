{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.PdfSurfaces
import Graphics.Cairo.Drawing.Paths

main :: IO ()
main = do
	((putStrLn <=< cairoPdfVersionToString) `mapM_`) =<< cairoPdfGetVersions
	cairoPdfSurfaceWith "game-6x7.pdf" 595 842 \sr -> do
		cr <- cairoCreate sr
		for_ (take 6 [60, 140 ..]) \x ->
			for_ (take 7 [150, 230 ..]) \y ->
				cairoRectangle cr x y 77 77
		cairoStroke cr
