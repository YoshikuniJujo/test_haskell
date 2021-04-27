{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TabStops where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Data.Word
import Data.Int
import System.IO.Unsafe

import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

foreign import ccall "pango_tab_array_new" c_pango_tab_array_new ::
	#{type gint} -> #{type gboolean} -> IO (Ptr (PangoTabArrayPrim s))

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

pangoTabArrayPangoUnitNew ::
	PrimMonad m => m (PangoTabArrayPangoUnit (PrimState m))
pangoTabArrayPangoUnitNew = unsafeIOToPrim
	$ mkPangoTabArrayPangoUnit =<< c_pango_tab_array_new 1 #{const FALSE}

pangoTabArrayPixelUnitNew ::
	PrimMonad m => m (PangoTabArrayPixelUnit (PrimState m))
pangoTabArrayPixelUnitNew = unsafeIOToPrim
	$ mkPangoTabArrayPixelUnit =<< c_pango_tab_array_new 1 #{const TRUE}

pangoTabArrayNew :: PrimMonad m =>
	#{type gint} -> Bool -> m (PangoTabArrayPrim (PrimState m))
pangoTabArrayNew sz px = unsafeIOToPrim
	$ makePangoTabArrayPrim =<< c_pango_tab_array_new sz (boolToGboolean px)

{-
pangoTabArrayPangoUnitSetTab :: PrimMonad m =>
	PangoTabArrayPangoUnit (PrimState m) -> CInt -> TabInPangoUnit -> m ()
pangoTabArrayPangoUnitSetTab
-}

foreign import ccall "pango_tab_array_get_size" c_pango_tab_array_get_size ::
	Ptr PangoTabArray -> IO #type gint

pangoTabArrayGetSize :: PangoTabArray -> #type gint
pangoTabArrayGetSize (PangoTabArray fpta) = unsafePerformIO
	$ withForeignPtr fpta \pta -> c_pango_tab_array_get_size pta

foreign import ccall "pango_tab_array_resize" c_pango_tab_array_resize ::
	Ptr (PangoTabArrayPrim s) -> #{type gint} -> IO ()

pangoTabArrayResize :: PrimMonad m =>
	PangoTabArrayPrim (PrimState m) -> #{type gint} -> m ()
pangoTabArrayResize (PangoTabArrayPrim fpta) sz = unsafeIOToPrim
	$ withForeignPtr fpta \pta -> c_pango_tab_array_resize pta sz

foreign import ccall "pango_tab_array_set_tab" c_pango_tab_array_set_tab ::
	Ptr (PangoTabArrayPrim s) -> #{type gint} -> #{type PangoTabAlign} -> #{type gint} -> IO ()

pangoTabArraySetTab :: PrimMonad m =>
	PangoTabArrayPrim (PrimState m) -> #{type gint} -> PangoTabAlign -> #{type gint} -> m ()
pangoTabArraySetTab (PangoTabArrayPrim fpta) idx (PangoTabAlign algn) loc = unsafeIOToPrim
	$ withForeignPtr fpta \pta -> c_pango_tab_array_set_tab pta idx algn loc

foreign import ccall "pango_tab_array_get_tab" c_pango_tab_array_get_tab ::
	Ptr PangoTabArray -> #{type gint} -> Ptr #{type PangoTabAlign} -> Ptr #{type gint} -> IO ()

pangoTabArrayGetTab ::
	PangoTabArray -> #{type gint} -> (PangoTabAlign, #type gint)
pangoTabArrayGetTab (PangoTabArray fpta) idx = unsafePerformIO
	$ withForeignPtr fpta \pta -> alloca \aln -> alloca \loc -> do
		c_pango_tab_array_get_tab pta idx aln loc
		(,) <$> (PangoTabAlign <$> peek aln) <*> peek loc

foreign import ccall "pango_tab_array_get_tabs" c_pango_tab_array_get_tabs ::
	Ptr PangoTabArray -> Ptr (Ptr #type PangoTabAlign) -> Ptr (Ptr #type gint) -> IO ()

pangoTabArrayGetTabs :: PangoTabArray -> [(PangoTabAlign, #type gint)]
pangoTabArrayGetTabs (PangoTabArray fpta) = unsafePerformIO
	$ withForeignPtr fpta \pta -> alloca \algn -> alloca \loc -> do
		n <- c_pango_tab_array_get_size pta
		c_pango_tab_array_get_tabs pta algn loc
		zip	<$> (map PangoTabAlign <$> (peekArrayAndFree n =<< peek algn))
			<*> (peekArrayAndFree n =<< peek loc)

peekArrayAndFree :: Storable a => #{type gint} -> Ptr a -> IO [a]
peekArrayAndFree n p = peekArray (fromIntegral n) p <* free p

foreign import ccall "pango_tab_array_get_positions_in_pixels"
	c_pango_tab_array_get_positions_in_pixels ::
	Ptr PangoTabArray -> IO #type gboolean

pangoTabArrayGetPositionsInPixels :: PangoTabArray -> Bool
pangoTabArrayGetPositionsInPixels (PangoTabArray fpta) = unsafePerformIO
	$ withForeignPtr fpta \pta ->
		gbooleanToBool <$> c_pango_tab_array_get_positions_in_pixels pta

foreign import ccall "pango_tab_array_copy" c_pango_tab_array_freeze ::
	Ptr (PangoTabArrayPrim s) -> IO (Ptr PangoTabArray)

foreign import ccall "pango_tab_array_copy" c_pango_tab_array_thaw ::
	Ptr PangoTabArray -> IO (Ptr (PangoTabArrayPrim s))

pangoTabArrayFreeze :: PrimMonad m =>
	PangoTabArrayPrim (PrimState m) -> m PangoTabArray
pangoTabArrayFreeze (PangoTabArrayPrim fpta) = unsafeIOToPrim
	$ withForeignPtr fpta \pta -> makePangoTabArray =<< c_pango_tab_array_freeze pta

pangoTabArrayThaw :: PrimMonad m =>
	PangoTabArray -> m (PangoTabArrayPrim (PrimState m))
pangoTabArrayThaw (PangoTabArray fpta) = unsafeIOToPrim
	$ withForeignPtr fpta \pta -> makePangoTabArrayPrim =<< c_pango_tab_array_thaw pta
