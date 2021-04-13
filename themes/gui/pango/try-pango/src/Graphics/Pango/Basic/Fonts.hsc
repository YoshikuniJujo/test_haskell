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
import System.IO.Unsafe

import Graphics.Pango.Monad
import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

pangoFontDescriptionNew :: PrimMonad m => m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionNew = unPrimIo
	$ makePangoFontDescriptionPrim =<< c_pango_font_description_new

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr (PangoFontDescriptionPrim s))

foreign import ccall "pango_font_description_copy" c_pango_font_description_copy ::
	Ptr (PangoFontDescriptionPrim s) -> IO (Ptr (PangoFontDescriptionPrim s))

pangoFontDescriptionCopy :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionCopy (PangoFontDescriptionPrim fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		makePangoFontDescriptionPrim =<< c_pango_font_description_copy pfd

foreign import ccall "pango_font_description_copy_static" c_pango_font_description_copy_static ::
	Ptr (PangoFontDescriptionPrim s) -> IO (Ptr (PangoFontDescriptionPrim s))

pangoFontDescriptionCopyStatic :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionCopyStatic (PangoFontDescriptionPrim fpfd) = unPrimIo
	$ withForeignPtr fpfd \pfd -> do
		p <- c_pango_font_description_copy_static pfd
		PangoFontDescriptionPrim <$> newForeignPtr p
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
	Ptr (PangoFontDescriptionPrim s) -> CString -> IO ()

pangoFontDescriptionSetFamily :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamily (PangoFontDescriptionPrim fpfd) f = unPrimIo
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family_static" c_pango_font_description_set_family_static ::
	Ptr (PangoFontDescriptionPrim s) -> CString -> IO ()

newForeignCString :: String -> IO (ForeignPtr CChar)
newForeignCString s = do
	p <- newCString s
	newForeignPtr p (free p)

pangoFontDescriptionSetFamilyStatic :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamilyStatic (PangoFontDescriptionPrim fpfd) f = unPrimIo do
	fcf <- newForeignCString f
	addForeignPtrFinalizer fpfd $ touchForeignPtr fcf
	withForeignPtr fpfd \pfd -> withForeignPtr fcf \cf ->
		c_pango_font_description_set_family_static pfd cf

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr PangoFontDescription -> IO CString

pangoFontDescriptionGetFamily :: PangoFontDescription -> String
pangoFontDescriptionGetFamily (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoStyle} -> IO ()

pangoFontDescriptionSetStyle :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoStyle -> m ()
pangoFontDescriptionSetStyle (PangoFontDescriptionPrim fpfd) (PangoStyle s) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr PangoFontDescription -> IO #type PangoStyle

pangoFontDescriptionGetStyle :: PangoFontDescription -> PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoVariant} -> IO ()

pangoFontDescriptionSetVariant :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoVariant -> m ()
pangoFontDescriptionSetVariant (PangoFontDescriptionPrim fpfd) (PangoVariant pv) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr PangoFontDescription -> IO #type PangoVariant

pangoFontDescriptionGetVariant :: PangoFontDescription -> PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoWeight} -> IO ()

pangoFontDescriptionSetWeight :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoWeight -> m ()
pangoFontDescriptionSetWeight (PangoFontDescriptionPrim fpfd) (PangoWeight wt) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr PangoFontDescription -> IO #type PangoWeight

pangoFontDescriptionGetWeight :: PangoFontDescription -> PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoStretch} -> IO ()

pangoFontDescriptionSetStretch :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoStretch -> m ()
pangoFontDescriptionSetStretch (PangoFontDescriptionPrim fpfd) (PangoStretch ps) = unPrimIo
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

foreign import ccall "pango_font_description_get_stretch" c_pango_font_description_get_stretch ::
	Ptr PangoFontDescription -> IO #type PangoStretch

pangoFontDescriptionGetStretch :: PangoFontDescription -> PangoStretch
pangoFontDescriptionGetStretch (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd -> PangoStretch <$> c_pango_font_description_get_stretch pfd

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr (PangoFontDescriptionPrim s) -> #{type gint} -> IO ()

pangoFontDescriptionSetSize :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> #{type gint} -> m ()
pangoFontDescriptionSetSize (PangoFontDescriptionPrim fpfd) n = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr PangoFontDescription -> IO #type gint

pangoFontDescriptionGetSize :: PangoFontDescription -> #type gint
pangoFontDescriptionGetSize (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr (PangoFontDescriptionPrim s) -> #{type double} -> IO ()

pangoFontDescriptionSetAbsoluteSize :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> #{type double} -> m ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescriptionPrim fpfd) sz = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz

foreign import ccall "pango_font_description_get_size_is_absolute"
	c_pango_font_description_get_size_is_absolute ::
	Ptr PangoFontDescription -> IO #type gboolean

pangoFontDescriptionGetSizeIsAbsolute :: PangoFontDescription -> Bool
pangoFontDescriptionGetSizeIsAbsolute (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		gbooleanToBool <$> c_pango_font_description_get_size_is_absolute pfd

foreign import ccall "pango_font_description_set_gravity"
	c_pango_font_description_set_gravity ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoGravity} -> IO ()

pangoFontDescriptionSetGravity :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoGravity -> m ()
pangoFontDescriptionSetGravity (PangoFontDescriptionPrim fpfd) (PangoGravity gr) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_gravity pfd gr

foreign import ccall "pango_font_description_get_gravity"
	c_pango_font_description_get_gravity ::
	Ptr PangoFontDescription -> IO #type PangoGravity

pangoFontDescriptionGetGravity :: PangoFontDescription -> PangoGravity
pangoFontDescriptionGetGravity (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoGravity <$> c_pango_font_description_get_gravity pfd

foreign import ccall "pango_font_description_get_set_fields"
	c_pango_font_description_get_set_fields ::
	Ptr PangoFontDescription -> IO #type PangoFontMask

pangoFontDescriptionGetSetFields :: PangoFontDescription -> PangoFontMask
pangoFontDescriptionGetSetFields (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoFontMask <$> c_pango_font_description_get_set_fields pfd

foreign import ccall "pango_font_description_unset_fields"
	c_pango_font_description_unset_fields ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoFontMask} -> IO ()

pangoFontDescriptionUnsetFields :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoFontMask -> m ()
pangoFontDescriptionUnsetFields (PangoFontDescriptionPrim fpfd) (PangoFontMask msk) = unPrimIo
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_unset_fields pfd msk

foreign import ccall "pango_font_description_to_string"
	c_pango_font_description_to_string ::
	Ptr PangoFontDescription -> IO CString

pangoFontDescriptionToString :: PangoFontDescription -> String
pangoFontDescriptionToString (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_string pfd

foreign import ccall "pango_font_description_to_filename"
	c_pango_font_description_to_filename ::
	Ptr PangoFontDescription -> IO CString

pangoFontDescriptionToFilename :: PangoFontDescription -> String
pangoFontDescriptionToFilename (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_filename pfd
