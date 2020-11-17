{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.LowLevel.TabStops where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Data.Word
import Data.Int

import Graphics.Pango.Monad
import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

foreign import ccall "pango_tab_array_new" c_pango_tab_array_new ::
	#{type gint} -> #{type gboolean} -> IO (Ptr (PangoTabArray s))

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

pangoTabArrayNew :: PrimMonad m =>
	#{type gint} -> Bool -> m (PangoTabArray (PrimState m))
pangoTabArrayNew sz px = unPrimIo
	$ makePangoTabArray =<< c_pango_tab_array_new sz (boolToGboolean px)

foreign import ccall "pango_tab_array_get_size" c_pango_tab_array_get_size ::
	Ptr (PangoTabArray s) -> IO #type gint

pangoTabArrayGetSize :: PrimMonad m =>
	PangoTabArray (PrimState m) -> m #type gint
pangoTabArrayGetSize (PangoTabArray fpta) = unPrimIo
	$ withForeignPtr fpta \pta -> c_pango_tab_array_get_size pta

foreign import ccall "pango_tab_array_resize" c_pango_tab_array_resize ::
	Ptr (PangoTabArray s) -> #{type gint} -> IO ()

pangoTabArrayResize :: PrimMonad m =>
	PangoTabArray (PrimState m) -> #{type gint} -> m ()
pangoTabArrayResize (PangoTabArray fpta) sz = unPrimIo
	$ withForeignPtr fpta \pta -> c_pango_tab_array_resize pta sz

foreign import ccall "pango_tab_array_set_tab" c_pango_tab_array_set_tab ::
	Ptr (PangoTabArray s) -> #{type gint} -> #{type PangoTabAlign} -> #{type gint} -> IO ()

pangoTabArraySetTab :: PrimMonad m =>
	PangoTabArray (PrimState m) -> #{type gint} -> PangoTabAlign -> #{type gint} -> m ()
pangoTabArraySetTab (PangoTabArray fpta) idx (PangoTabAlign algn) loc = unPrimIo
	$ withForeignPtr fpta \pta -> c_pango_tab_array_set_tab pta idx algn loc

foreign import ccall "pango_tab_array_get_tab" c_pango_tab_array_get_tab ::
	Ptr (PangoTabArray s) -> #{type gint} -> Ptr #{type PangoTabAlign} -> Ptr #{type gint} -> IO ()

pangoTabArrayGetTab :: PrimMonad m =>
	PangoTabArray (PrimState m) -> #{type gint} -> m (PangoTabAlign, #type gint)
pangoTabArrayGetTab (PangoTabArray fpta) idx = unPrimIo
	$ withForeignPtr fpta \pta -> alloca \aln -> alloca \loc -> do
		c_pango_tab_array_get_tab pta idx aln loc
		(,) <$> (PangoTabAlign <$> peek aln) <*> peek loc

foreign import ccall "pango_tab_array_get_tabs" c_pango_tab_array_get_tabs ::
	Ptr (PangoTabArray s) -> Ptr (Ptr #type PangoTabAlign) -> Ptr (Ptr #type gint) -> IO ()

pangoTabArrayGetTabs :: PrimMonad m =>
	PangoTabArray (PrimState m) -> m [(PangoTabAlign, #type gint)]
pangoTabArrayGetTabs (PangoTabArray fpta) = unPrimIo
	$ withForeignPtr fpta \pta -> alloca \algn -> alloca \loc -> do
		n <- c_pango_tab_array_get_size pta
		c_pango_tab_array_get_tabs pta algn loc
		zip	<$> (map PangoTabAlign <$> (peekArrayAndFree n =<< peek algn))
			<*> (peekArrayAndFree n =<< peek loc)

peekArrayAndFree :: Storable a => #{type gint} -> Ptr a -> IO [a]
peekArrayAndFree n p = peekArray (fromIntegral n) p <* free p
