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

import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr PangoFontDescription)

pangoFontDescriptionNew :: IO PangoFontDescription
pangoFontDescriptionNew =
	makePangoFontDescription =<< c_pango_font_description_new

foreign import ccall "pango_font_description_copy" c_pango_font_description_copy ::
	Ptr PangoFontDescription -> IO (Ptr PangoFontDescription)

pangoFontDescriptionCopy :: PangoFontDescription -> IO PangoFontDescription
pangoFontDescriptionCopy (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		makePangoFontDescription =<< c_pango_font_description_copy pfd

foreign import ccall "pango_font_description_copy_static" c_pango_font_description_copy_static ::
	Ptr PangoFontDescription -> IO (Ptr PangoFontDescription)

pangoFontDescriptionCopyStatic :: PangoFontDescription -> IO PangoFontDescription
pangoFontDescriptionCopyStatic (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd -> do
		p <- c_pango_font_description_copy_static pfd
		PangoFontDescription <$> newForeignPtr p (touchForeignPtr fpfd >> c_pango_font_description_free p)

foreign import ccall "pango_font_description_equal" c_pango_font_description_equal ::
	Ptr PangoFontDescription -> Ptr PangoFontDescription -> IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

pangoFontDescriptionEqual :: PangoFontDescription -> PangoFontDescription -> IO Bool
pangoFontDescriptionEqual (PangoFontDescription fpfd1) (PangoFontDescription fpfd2) =
	withForeignPtr fpfd1 \pfd1 -> withForeignPtr fpfd2 \pfd2 ->
		gbooleanToBool <$> c_pango_font_description_equal pfd1 pfd2

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr PangoFontDescription -> CString -> IO ()

pangoFontDescriptionSetFamily :: PangoFontDescription -> String -> IO ()
pangoFontDescriptionSetFamily (PangoFontDescription fpfd) f =
	withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family_static" c_pango_font_description_set_family_static ::
	Ptr PangoFontDescription -> CString -> IO ()

newForeignCString :: String -> IO (ForeignPtr CChar)
newForeignCString s = do
	p <- newCString s
	newForeignPtr p (free p)

pangoFontDescriptionSetFamilyStatic :: PangoFontDescription -> String -> IO ()
pangoFontDescriptionSetFamilyStatic (PangoFontDescription fpfd) f = do
	fcf <- newForeignCString f
	addForeignPtrFinalizer fpfd $ touchForeignPtr fcf
	withForeignPtr fpfd \pfd -> withForeignPtr fcf \cf ->
		c_pango_font_description_set_family_static pfd cf

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr PangoFontDescription -> IO CString

pangoFontDescriptionGetFamily :: PangoFontDescription -> IO String
pangoFontDescriptionGetFamily (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr PangoFontDescription -> #{type PangoStyle} -> IO ()

pangoFontDescriptionSetStyle :: PangoFontDescription -> PangoStyle -> IO ()
pangoFontDescriptionSetStyle (PangoFontDescription fpfd) (PangoStyle s) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr PangoFontDescription -> IO #type PangoStyle

pangoFontDescriptionGetStyle :: PangoFontDescription -> IO PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr PangoFontDescription -> #{type PangoVariant} -> IO ()

pangoFontDescriptionSetVariant :: PangoFontDescription -> PangoVariant -> IO ()
pangoFontDescriptionSetVariant (PangoFontDescription fpfd) (PangoVariant pv) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr PangoFontDescription -> IO #type PangoVariant

pangoFontDescriptionGetVariant :: PangoFontDescription -> IO PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr PangoFontDescription -> #{type PangoWeight} -> IO ()

pangoFontDescriptionSetWeight :: PangoFontDescription -> PangoWeight -> IO ()
pangoFontDescriptionSetWeight (PangoFontDescription fpfd) (PangoWeight wt) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr PangoFontDescription -> IO #type PangoWeight

pangoFontDescriptionGetWeight :: PangoFontDescription -> IO PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr PangoFontDescription -> #{type PangoStretch} -> IO ()

pangoFontDescriptionSetStretch :: PangoFontDescription -> PangoStretch -> IO ()
pangoFontDescriptionSetStretch (PangoFontDescription fpfd) (PangoStretch ps) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

foreign import ccall "pango_font_description_get_stretch" c_pango_font_description_get_stretch ::
	Ptr PangoFontDescription -> IO #type PangoStretch

pangoFontDescriptionGetStretch :: PangoFontDescription -> IO PangoStretch
pangoFontDescriptionGetStretch (PangoFontDescription fpfd) =
	withForeignPtr fpfd \pfd -> PangoStretch <$> c_pango_font_description_get_stretch pfd

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr PangoFontDescription -> #{type gint} -> IO ()

pangoFontDescriptionSetSize :: PangoFontDescription -> #{type gint} -> IO ()
pangoFontDescriptionSetSize (PangoFontDescription fpfd) n =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr PangoFontDescription -> IO #type gint

pangoFontDescriptionGetSize :: PangoFontDescription -> IO #type gint
pangoFontDescriptionGetSize (PangoFontDescription fpfd) =
	withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr PangoFontDescription -> #{type double} -> IO ()

pangoFontDescriptionSetAbsoluteSize :: PangoFontDescription -> #{type double} -> IO ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescription fpfd) sz =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz
