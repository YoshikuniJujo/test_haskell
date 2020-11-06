{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts where

import Foreign.Ptr
import Control.Monad.Primitive

import Graphics.Pango.Monad
import Graphics.Pango.Types

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr (PangoFontDescription s))

pangoFontDescriptionNew :: PrimMonad m => m (PangoFontDescription (PrimState m))
pangoFontDescriptionNew = unPrimIo
	$ makePangoFontDescription =<< c_pango_font_description_new
