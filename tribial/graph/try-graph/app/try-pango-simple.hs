{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Monad.ST
import Data.CairoImage.Internal
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Data.CairoContext

import qualified Data.Text as T

main :: IO ()
main = getArgs >>= \case
	sz : x : y : _ -> do
		s <- cairoImageSurfaceCreate CairoFormatArgb32 300 400
		cr <- cairoCreate s

		putText cr (Size $ read sz) (read x) (read y) "Hello"

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-simple.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "no family"

putText :: CairoT s RealWorld -> Size -> CDouble -> CDouble -> T.Text -> IO ()
putText cr sz x y txt = do
	pl <- pangoCairoCreateLayout cr
	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Family "sans"
	pangoFontDescriptionSet fd sz
	fd' <- pangoFontDescriptionFreeze fd
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet pl txt
	cairoIdentityMatrix cr
	cairoTranslate cr x y
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
