{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryPango where

import Foreign.C.Types
import Data.CairoContext

import Graphics.Cairo.Drawing.Paths
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

import Data.ImageData.Text

drawFont :: CairoTIO s -> CDouble -> CDouble -> Font -> T.Text -> IO ()
drawFont cr x y (Font fn) t = do
	pl <- pangoCairoCreateLayout cr

	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Family fn
	pangoFontDescriptionSet fd $ Size 32
	fd' <- pangoFontDescriptionFreeze fd

	cairoMoveTo cr x y
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl t

	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
