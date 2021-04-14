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

pangoFontDescriptionNew :: PrimMonad m => m (PangoFontDescription (PrimState m))
pangoFontDescriptionNew = unPrimIo
	$ makePangoFontDescription =<< c_pango_font_description_new

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr (PangoFontDescription s))

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
		PangoFontDescription <$> newForeignPtr p
			(touchForeignPtr fpfd >> c_pango_font_description_prim_free p)

{-
pangoFontDescriptionEqual :: PangoFontDescription -> PangoFontDescription -> Bool
pangoFontDescriptionEqual (PangoFontDescription fpfd1) (PangoFontDescription fpfd2) = unsafePerformIO
	$ withForeignPtr fpfd1 \pfd1 -> withForeignPtr fpfd2 \pfd2 ->
		gbooleanToBool <$> c_pango_font_description_equal pfd1 pfd2

foreign import ccall "pango_font_description_equal" c_pango_font_description_equal ::
	Ptr PangoFontDescription -> Ptr PangoFontDescription -> IO #type gboolean
-}

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

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

pangoFontDescriptionGetFamily :: PrimMonad m => PangoFontDescription (PrimState m) -> m String
pangoFontDescriptionGetFamily (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr (PangoFontDescription s) -> IO CString

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr (PangoFontDescription s) -> #{type PangoStyle} -> IO ()

pangoFontDescriptionSetStyle :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoStyle -> m ()
pangoFontDescriptionSetStyle (PangoFontDescription fpfd) (PangoStyle s) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

pangoFontDescriptionGetStyle :: PrimMonad m => PangoFontDescription (PrimState m) -> m PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr (PangoFontDescription s) -> IO #type PangoStyle

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr (PangoFontDescription s) -> #{type PangoVariant} -> IO ()

pangoFontDescriptionSetVariant :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoVariant -> m ()
pangoFontDescriptionSetVariant (PangoFontDescription fpfd) (PangoVariant pv) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

pangoFontDescriptionGetVariant :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr (PangoFontDescription s) -> IO #type PangoVariant

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr (PangoFontDescription s) -> #{type PangoWeight} -> IO ()

pangoFontDescriptionSetWeight :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoWeight -> m ()
pangoFontDescriptionSetWeight (PangoFontDescription fpfd) (PangoWeight wt) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

pangoFontDescriptionGetWeight :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr (PangoFontDescription s) -> IO #type PangoWeight

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr (PangoFontDescription s) -> #{type PangoStretch} -> IO ()

pangoFontDescriptionSetStretch :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoStretch -> m ()
pangoFontDescriptionSetStretch (PangoFontDescription fpfd) (PangoStretch ps) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

pangoFontDescriptionGetStretch :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoStretch
pangoFontDescriptionGetStretch (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> PangoStretch <$> c_pango_font_description_get_stretch pfd

foreign import ccall "pango_font_description_get_stretch" c_pango_font_description_get_stretch ::
	Ptr (PangoFontDescription s) -> IO #type PangoStretch

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr (PangoFontDescription s) -> #{type gint} -> IO ()

pangoFontDescriptionSetSize :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> #{type gint} -> m ()
pangoFontDescriptionSetSize (PangoFontDescription fpfd) n = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

pangoFontDescriptionGetSize :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m #type gint
pangoFontDescriptionGetSize (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr (PangoFontDescription s) -> IO #type gint

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr (PangoFontDescription s) -> #{type double} -> IO ()

pangoFontDescriptionSetAbsoluteSize :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> #{type double} -> m ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescription fpfd) sz = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz

pangoFontDescriptionGetSizeIsAbsolute :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m Bool
pangoFontDescriptionGetSizeIsAbsolute (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		gbooleanToBool <$> c_pango_font_description_get_size_is_absolute pfd

foreign import ccall "pango_font_description_get_size_is_absolute"
	c_pango_font_description_get_size_is_absolute ::
	Ptr (PangoFontDescription s) -> IO #type gboolean

foreign import ccall "pango_font_description_set_gravity"
	c_pango_font_description_set_gravity ::
	Ptr (PangoFontDescription s) -> #{type PangoGravity} -> IO ()

pangoFontDescriptionSetGravity :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoGravity -> m ()
pangoFontDescriptionSetGravity (PangoFontDescription fpfd) (PangoGravity gr) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_gravity pfd gr

pangoFontDescriptionGetGravity :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoGravity
pangoFontDescriptionGetGravity (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoGravity <$> c_pango_font_description_get_gravity pfd

foreign import ccall "pango_font_description_get_gravity"
	c_pango_font_description_get_gravity ::
	Ptr (PangoFontDescription s) -> IO #type PangoGravity

pangoFontDescriptionGetSetFields :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoFontMask
pangoFontDescriptionGetSetFields (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoFontMask <$> c_pango_font_description_get_set_fields pfd

foreign import ccall "pango_font_description_get_set_fields"
	c_pango_font_description_get_set_fields ::
	Ptr (PangoFontDescription s) -> IO #type PangoFontMask

foreign import ccall "pango_font_description_unset_fields"
	c_pango_font_description_unset_fields ::
	Ptr (PangoFontDescription s) -> #{type PangoFontMask} -> IO ()

pangoFontDescriptionUnsetFields :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoFontMask -> m ()
pangoFontDescriptionUnsetFields (PangoFontDescription fpfd) (PangoFontMask msk) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_unset_fields pfd msk

pangoFontDescriptionToString :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m String
pangoFontDescriptionToString (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_string pfd

foreign import ccall "pango_font_description_to_string"
	c_pango_font_description_to_string ::
	Ptr (PangoFontDescription s) -> IO CString

pangoFontDescriptionToFilename :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m String
pangoFontDescriptionToFilename (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_filename pfd

foreign import ccall "pango_font_description_to_filename"
	c_pango_font_description_to_filename ::
	Ptr (PangoFontDescription s) -> IO CString
