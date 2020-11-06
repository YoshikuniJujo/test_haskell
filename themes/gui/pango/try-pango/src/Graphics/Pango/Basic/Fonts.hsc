{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad.Primitive
import Data.Int

import Graphics.Pango.Monad
import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr (PangoFontDescription s))

pangoFontDescriptionNew :: PrimMonad m => m (PangoFontDescription (PrimState m))
pangoFontDescriptionNew = unPrimIo
	$ makePangoFontDescription =<< c_pango_font_description_new

foreign import ccall "pango_font_description_copy" c_pango_font_description_copy ::
	Ptr (PangoFontDescription s) -> IO (Ptr (PangoFontDescription s))

pangoFontDescriptionCopy :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m (PangoFontDescription (PrimState m))
pangoFontDescriptionCopy (PangoFontDescription fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		makePangoFontDescription =<< c_pango_font_description_copy pfd

foreign import ccall "pango_font_description_copy_static" c_pango_font_description_copy_static ::
	Ptr (PangoFontDescription s) -> IO (Ptr (PangoFontDescription s))

pangoFontDescriptionCopyStatic :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m (PangoFontDescription (PrimState m))
pangoFontDescriptionCopyStatic (PangoFontDescription fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd -> do
		p <- c_pango_font_description_copy_static pfd
		PangoFontDescription <$> newForeignPtr p (touchForeignPtr fpfd >> c_pango_font_description_free p)

foreign import ccall "pango_font_description_equal" c_pango_font_description_equal ::
	Ptr (PangoFontDescription s) -> Ptr (PangoFontDescription s) -> IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

pangoFontDescriptionEqual :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoFontDescription (PrimState m) -> m Bool
pangoFontDescriptionEqual (PangoFontDescription fpfd1) (PangoFontDescription fpfd2) = unPrimIo
	$ withForeignPtr fpfd1 \pfd1 -> withForeignPtr fpfd2 \pfd2 ->
		gbooleanToBool <$> c_pango_font_description_equal pfd1 pfd2
