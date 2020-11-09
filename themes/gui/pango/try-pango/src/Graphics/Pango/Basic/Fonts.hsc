{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C
import Data.Word
import Data.Int

import Graphics.Pango.Monad
import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr (PangoFontDescription s))

pangoFontDescriptionNew :: IO (PangoFontDescription s)
pangoFontDescriptionNew =
	makePangoFontDescription =<< c_pango_font_description_new

foreign import ccall "pango_font_description_copy" c_pango_font_description_copy ::
	Ptr (PangoFontDescription s) -> IO (Ptr (PangoFontDescription s))

pangoFontDescriptionCopy ::
	PangoFontDescription s -> IO (PangoFontDescription s)
pangoFontDescriptionCopy (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		makePangoFontDescription =<< c_pango_font_description_copy pfd

foreign import ccall "pango_font_description_copy_static" c_pango_font_description_copy_static ::
	Ptr (PangoFontDescription s) -> IO (Ptr (PangoFontDescription s))

pangoFontDescriptionCopyStatic ::
	PangoFontDescription s -> IO (PangoFontDescription s)
pangoFontDescriptionCopyStatic (PangoFontDescription fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd -> do
		p <- c_pango_font_description_copy_static pfd
		PangoFontDescription <$> newForeignPtr p (touchForeignPtr fpfd >> c_pango_font_description_free p)

foreign import ccall "pango_font_description_equal" c_pango_font_description_equal ::
	Ptr (PangoFontDescription s) -> Ptr (PangoFontDescription s) -> IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

pangoFontDescriptionEqual ::
	PangoFontDescription s -> PangoFontDescription s -> IO Bool
pangoFontDescriptionEqual (PangoFontDescription fpfd1) (PangoFontDescription fpfd2) = unPrimIo
	$ withForeignPtr fpfd1 \pfd1 -> withForeignPtr fpfd2 \pfd2 ->
		gbooleanToBool <$> c_pango_font_description_equal pfd1 pfd2

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr (PangoFontDescription s) -> CString -> IO ()

pangoFontDescriptionSetFamily :: PangoFontDescription s -> String -> IO ()
pangoFontDescriptionSetFamily (PangoFontDescription fpfd) f = unPrimIo
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family_static" c_pango_font_description_set_family_static ::
	Ptr (PangoFontDescription s) -> CString -> IO ()

newForeignCString :: String -> IO (ForeignPtr CChar)
newForeignCString s = do
	p <- newCString s
	newForeignPtr p (free p)

pangoFontDescriptionSetFamilyStatic :: PangoFontDescription s -> String -> IO ()
pangoFontDescriptionSetFamilyStatic (PangoFontDescription fpfd) f = unPrimIo do
	fcf <- newForeignCString f
	addForeignPtrFinalizer fpfd $ touchForeignPtr fcf
	withForeignPtr fpfd \pfd -> withForeignPtr fcf \cf ->
		c_pango_font_description_set_family_static pfd cf

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr (PangoFontDescription s) -> IO CString

pangoFontDescriptionGetFamily :: PangoFontDescription s -> IO String
pangoFontDescriptionGetFamily (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr (PangoFontDescription s) -> #{type PangoStyle} -> IO ()

pangoFontDescriptionSetStyle ::
	PangoFontDescription s -> PangoStyle -> IO ()
pangoFontDescriptionSetStyle (PangoFontDescription fpfd) (PangoStyle s) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr (PangoFontDescription s) -> IO #type PangoStyle

pangoFontDescriptionGetStyle :: PangoFontDescription s -> IO PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr (PangoFontDescription s) -> #{type PangoVariant} -> IO ()

pangoFontDescriptionSetVariant :: PangoFontDescription s -> PangoVariant -> IO ()
pangoFontDescriptionSetVariant (PangoFontDescription fpfd) (PangoVariant pv) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr (PangoFontDescription s) -> IO #type PangoVariant

pangoFontDescriptionGetVariant :: PangoFontDescription s -> IO PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr (PangoFontDescription s) -> #{type PangoWeight} -> IO ()

pangoFontDescriptionSetWeight ::
	PangoFontDescription s -> PangoWeight -> IO ()
pangoFontDescriptionSetWeight (PangoFontDescription fpfd) (PangoWeight wt) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr (PangoFontDescription s) -> IO #type PangoWeight

pangoFontDescriptionGetWeight :: PangoFontDescription s -> IO PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr (PangoFontDescription s) -> #{type PangoStretch} -> IO ()

pangoFontDescriptionSetStretch ::
	PangoFontDescription s -> PangoStretch -> IO ()
pangoFontDescriptionSetStretch (PangoFontDescription fpfd) (PangoStretch ps) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

foreign import ccall "pango_font_description_get_stretch" c_pango_font_description_get_stretch ::
	Ptr (PangoFontDescription s) -> IO #type PangoStretch

pangoFontDescriptionGetStretch :: PangoFontDescription s -> IO PangoStretch
pangoFontDescriptionGetStretch (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd -> PangoStretch <$> c_pango_font_description_get_stretch pfd

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr (PangoFontDescription s) -> #{type gint} -> IO ()

pangoFontDescriptionSetSize ::
	PangoFontDescription s -> #{type gint} -> IO ()
pangoFontDescriptionSetSize (PangoFontDescription fpfd) n =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr (PangoFontDescription s) -> IO #type gint

pangoFontDescriptionGetSize :: PangoFontDescription s -> IO #type gint
pangoFontDescriptionGetSize (PangoFontDescription fpfd) =
	withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr (PangoFontDescription s) -> #{type double} -> IO ()

pangoFontDescriptionSetAbsoluteSize ::
	PangoFontDescription s -> #{type double} -> IO ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescription fpfd) sz =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz
