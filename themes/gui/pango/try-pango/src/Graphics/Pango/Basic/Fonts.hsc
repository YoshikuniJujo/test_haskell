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
	IO (Ptr (PangoFontDescriptionPrim s))

pangoFontDescriptionNew :: PrimMonad m => m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionNew = unPrimIo
	$ makePangoFontDescriptionPrim =<< c_pango_font_description_new

foreign import ccall "pango_font_description_copy" c_pango_font_description_copy ::
	Ptr (PangoFontDescriptionPrim s) -> IO (Ptr (PangoFontDescriptionPrim s))

pangoFontDescriptionCopy :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionCopy (PangoFontDescriptionPrim fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		makePangoFontDescriptionPrim =<< c_pango_font_description_copy pfd

foreign import ccall "pango_font_description_copy_static" c_pango_font_description_copy_static ::
	Ptr PangoFontDescriptionOld -> IO (Ptr PangoFontDescriptionOld)

pangoFontDescriptionCopyStatic :: PangoFontDescriptionOld -> IO PangoFontDescriptionOld
pangoFontDescriptionCopyStatic (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd -> do
		p <- c_pango_font_description_copy_static pfd
		PangoFontDescriptionOld <$> newForeignPtr p (touchForeignPtr fpfd >> c_pango_font_description_old_free p)

foreign import ccall "pango_font_description_equal" c_pango_font_description_equal ::
	Ptr PangoFontDescriptionOld -> Ptr PangoFontDescriptionOld -> IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

pangoFontDescriptionEqual :: PangoFontDescriptionOld -> PangoFontDescriptionOld -> IO Bool
pangoFontDescriptionEqual (PangoFontDescriptionOld fpfd1) (PangoFontDescriptionOld fpfd2) =
	withForeignPtr fpfd1 \pfd1 -> withForeignPtr fpfd2 \pfd2 ->
		gbooleanToBool <$> c_pango_font_description_equal pfd1 pfd2

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr PangoFontDescriptionOld -> CString -> IO ()

pangoFontDescriptionSetFamily :: PangoFontDescriptionOld -> String -> IO ()
pangoFontDescriptionSetFamily (PangoFontDescriptionOld fpfd) f =
	withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family_static" c_pango_font_description_set_family_static ::
	Ptr PangoFontDescriptionOld -> CString -> IO ()

newForeignCString :: String -> IO (ForeignPtr CChar)
newForeignCString s = do
	p <- newCString s
	newForeignPtr p (free p)

pangoFontDescriptionSetFamilyStatic :: PangoFontDescriptionOld -> String -> IO ()
pangoFontDescriptionSetFamilyStatic (PangoFontDescriptionOld fpfd) f = do
	fcf <- newForeignCString f
	addForeignPtrFinalizer fpfd $ touchForeignPtr fcf
	withForeignPtr fpfd \pfd -> withForeignPtr fcf \cf ->
		c_pango_font_description_set_family_static pfd cf

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr PangoFontDescriptionOld -> IO CString

pangoFontDescriptionGetFamily :: PangoFontDescriptionOld -> IO String
pangoFontDescriptionGetFamily (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr PangoFontDescriptionOld -> #{type PangoStyle} -> IO ()

pangoFontDescriptionSetStyle :: PangoFontDescriptionOld -> PangoStyle -> IO ()
pangoFontDescriptionSetStyle (PangoFontDescriptionOld fpfd) (PangoStyle s) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr PangoFontDescriptionOld -> IO #type PangoStyle

pangoFontDescriptionGetStyle :: PangoFontDescriptionOld -> IO PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr PangoFontDescriptionOld -> #{type PangoVariant} -> IO ()

pangoFontDescriptionSetVariant :: PangoFontDescriptionOld -> PangoVariant -> IO ()
pangoFontDescriptionSetVariant (PangoFontDescriptionOld fpfd) (PangoVariant pv) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr PangoFontDescriptionOld -> IO #type PangoVariant

pangoFontDescriptionGetVariant :: PangoFontDescriptionOld -> IO PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr PangoFontDescriptionOld -> #{type PangoWeight} -> IO ()

pangoFontDescriptionSetWeight :: PangoFontDescriptionOld -> PangoWeight -> IO ()
pangoFontDescriptionSetWeight (PangoFontDescriptionOld fpfd) (PangoWeight wt) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr PangoFontDescriptionOld -> IO #type PangoWeight

pangoFontDescriptionGetWeight :: PangoFontDescriptionOld -> IO PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr PangoFontDescriptionOld -> #{type PangoStretch} -> IO ()

pangoFontDescriptionSetStretch :: PangoFontDescriptionOld -> PangoStretch -> IO ()
pangoFontDescriptionSetStretch (PangoFontDescriptionOld fpfd) (PangoStretch ps) =
	withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

foreign import ccall "pango_font_description_get_stretch" c_pango_font_description_get_stretch ::
	Ptr PangoFontDescriptionOld -> IO #type PangoStretch

pangoFontDescriptionGetStretch :: PangoFontDescriptionOld -> IO PangoStretch
pangoFontDescriptionGetStretch (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd -> PangoStretch <$> c_pango_font_description_get_stretch pfd

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr PangoFontDescriptionOld -> #{type gint} -> IO ()

pangoFontDescriptionSetSize :: PangoFontDescriptionOld -> #{type gint} -> IO ()
pangoFontDescriptionSetSize (PangoFontDescriptionOld fpfd) n =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr PangoFontDescriptionOld -> IO #type gint

pangoFontDescriptionGetSize :: PangoFontDescriptionOld -> IO #type gint
pangoFontDescriptionGetSize (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr PangoFontDescriptionOld -> #{type double} -> IO ()

pangoFontDescriptionSetAbsoluteSize :: PangoFontDescriptionOld -> #{type double} -> IO ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescriptionOld fpfd) sz =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz

foreign import ccall "pango_font_description_get_size_is_absolute"
	c_pango_font_description_get_size_is_absolute ::
	Ptr PangoFontDescriptionOld -> IO #type gboolean

pangoFontDescriptionGetSizeIsAbsolute :: PangoFontDescriptionOld -> IO Bool
pangoFontDescriptionGetSizeIsAbsolute (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		gbooleanToBool <$> c_pango_font_description_get_size_is_absolute pfd

foreign import ccall "pango_font_description_set_gravity"
	c_pango_font_description_set_gravity ::
	Ptr PangoFontDescriptionOld -> #{type PangoGravity} -> IO ()

pangoFontDescriptionSetGravity :: PangoFontDescriptionOld -> PangoGravity -> IO ()
pangoFontDescriptionSetGravity (PangoFontDescriptionOld fpfd) (PangoGravity gr) =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_gravity pfd gr

foreign import ccall "pango_font_description_get_gravity"
	c_pango_font_description_get_gravity ::
	Ptr PangoFontDescriptionOld -> IO #type PangoGravity

pangoFontDescriptionGetGravity :: PangoFontDescriptionOld -> IO PangoGravity
pangoFontDescriptionGetGravity (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoGravity <$> c_pango_font_description_get_gravity pfd

foreign import ccall "pango_font_description_get_set_fields"
	c_pango_font_description_get_set_fields ::
	Ptr PangoFontDescriptionOld -> IO #type PangoFontMask

pangoFontDescriptionGetSetFields :: PangoFontDescriptionOld -> IO PangoFontMask
pangoFontDescriptionGetSetFields (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		PangoFontMask <$> c_pango_font_description_get_set_fields pfd

foreign import ccall "pango_font_description_unset_fields"
	c_pango_font_description_unset_fields ::
	Ptr PangoFontDescriptionOld -> #{type PangoFontMask} -> IO ()

pangoFontDescriptionUnsetFields :: PangoFontDescriptionOld -> PangoFontMask -> IO ()
pangoFontDescriptionUnsetFields (PangoFontDescriptionOld fpfd) (PangoFontMask msk) =
	withForeignPtr fpfd \pfd ->
		c_pango_font_description_unset_fields pfd msk

foreign import ccall "pango_font_description_to_string"
	c_pango_font_description_to_string ::
	Ptr PangoFontDescriptionOld -> IO CString

pangoFontDescriptionToString :: PangoFontDescriptionOld -> IO String
pangoFontDescriptionToString (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_string pfd

foreign import ccall "pango_font_description_to_filename"
	c_pango_font_description_to_filename ::
	Ptr PangoFontDescriptionOld -> IO CString

pangoFontDescriptionToFilename :: PangoFontDescriptionOld -> IO String
pangoFontDescriptionToFilename (PangoFontDescriptionOld fpfd) =
	withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_filename pfd
