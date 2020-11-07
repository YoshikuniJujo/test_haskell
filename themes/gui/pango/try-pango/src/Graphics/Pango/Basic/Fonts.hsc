{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C
import Control.Monad.Primitive
import Data.Word
import Data.Int

import Graphics.Pango.Monad
import Graphics.Pango.Types
import Graphics.Pango.Values

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

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr (PangoFontDescription s) -> CString -> IO ()

pangoFontDescriptionSetFamily :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamily (PangoFontDescription fpfd) f = unPrimIo
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family_static" c_pango_font_description_set_family_static ::
	Ptr (PangoFontDescription s) -> CString -> IO ()

newForeignCString :: String -> IO (ForeignPtr CChar)
newForeignCString s = do
	p <- newCString s
	newForeignPtr p (free p)

pangoFontDescriptionSetFamilyStatic :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamilyStatic (PangoFontDescription fpfd) f = unPrimIo do
	fcf <- newForeignCString f
	addForeignPtrFinalizer fpfd $ touchForeignPtr fcf
	withForeignPtr fpfd \pfd -> withForeignPtr fcf \cf ->
		c_pango_font_description_set_family_static pfd cf

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr (PangoFontDescription s) -> IO CString

pangoFontDescriptionGetFamily :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m String
pangoFontDescriptionGetFamily (PangoFontDescription fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr (PangoFontDescription s) -> #{type PangoStyle} -> IO ()

pangoFontDescriptionSetStyle :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoStyle -> m ()
pangoFontDescriptionSetStyle (PangoFontDescription fpfd) (PangoStyle s) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr (PangoFontDescription s) -> IO #type PangoStyle

pangoFontDescriptionGetStyle :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescription fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr (PangoFontDescription s) -> #{type PangoVariant} -> IO ()

pangoFontDescriptionSetVariant :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoVariant -> m ()
pangoFontDescriptionSetVariant (PangoFontDescription fpfd) (PangoVariant pv) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr (PangoFontDescription s) -> IO #type PangoVariant

pangoFontDescriptionGetVariant :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescription fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr (PangoFontDescription s) -> #{type PangoWeight} -> IO ()

pangoFontDescriptionSetWeight :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoWeight -> m ()
pangoFontDescriptionSetWeight (PangoFontDescription fpfd) (PangoWeight wt) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr (PangoFontDescription s) -> IO #type PangoWeight

pangoFontDescriptionGetWeight :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescription fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr (PangoFontDescription s) -> #{type PangoStretch} -> IO ()

pangoFontDescriptionSetStretch :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoStretch -> m ()
pangoFontDescriptionSetStretch (PangoFontDescription fpfd) (PangoStretch ps) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps
