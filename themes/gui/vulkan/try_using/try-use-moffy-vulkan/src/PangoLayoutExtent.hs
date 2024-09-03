{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PangoLayoutExtent (
	getPangoLayoutExtent, PixelExtents(..),

	PangoRectanglePixel,
	pangoRectanglePixelX, pangoRectanglePixelY,
	pangoRectanglePixelWidth, pangoRectanglePixelHeight,
	) where

import Foreign.C.Types
import Control.Monad.ST
import Data.Text qualified as T
import Data.CairoContext
import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

getPangoLayoutExtent ::
	CairoT r RealWorld -> String -> CDouble -> T.Text -> IO PixelExtents
getPangoLayoutExtent cr fm sz tx = do
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd $ Family fm
	pangoFontDescriptionSet pfd $ AbsoluteSize sz
	pangoLayoutSet pl . pangoFontDescriptionToNullable . Just
		=<< pangoFontDescriptionFreeze pfd
	pangoLayoutSet pl tx
	pangoLayoutInfo @PixelExtents <$> pangoLayoutFreeze pl
