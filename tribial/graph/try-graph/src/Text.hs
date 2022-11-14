{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Text (putText, putTextRot90, Size(..)) where

import Foreign.C.Types
import Control.Monad.ST
import Data.Text qualified as T
import Data.CairoContext
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

putText :: CairoT s RealWorld -> Size -> CDouble -> CDouble -> T.Text -> IO ()
putText cr sz x y txt = do
	pl <- pangoCairoCreateLayout cr
	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Family "sans"
	pangoFontDescriptionSet fd sz
	fd' <- pangoFontDescriptionFreeze fd
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet pl txt
	cairoMoveTo cr x y
--	cairoIdentityMatrix cr
--	cairoTranslate cr x y
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
--	cairoIdentityMatrix cr

putTextRot90 :: CairoT s RealWorld -> Size -> CDouble -> CDouble -> T.Text -> IO ()
putTextRot90 cr sz x y txt = do
--	cairoTranslate cr (- x) (- y)
	cairoRotate cr (- pi / 2)
--	cairoTranslate cr x y
--	putText cr sz 0 0 txt
	putText cr sz (- y) x txt
	cairoIdentityMatrix cr
