{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Monad.Primitive
import Data.CairoContext

cairoTagBegin :: PrimMonad m => CairoT (PrimState m) -> String -> String -> m ()
cairoTagBegin (CairoT fcr) nm attr = unsafeIOToPrim $ withForeignPtr fcr \pcr ->
	withCString nm \cnm -> withCString attr \cattr ->
		c_cairo_tag_begin pcr cnm cattr

foreign import ccall "cairo_tag_begin" c_cairo_tag_begin ::
	Ptr (CairoT s) -> CString -> CString -> IO ()

cairoTagEnd :: PrimMonad m => CairoT (PrimState m) -> String -> m ()
cairoTagEnd (CairoT fcr) nm = unsafeIOToPrim $ withForeignPtr fcr \pcr ->
	withCString nm \cnm -> c_cairo_tag_end pcr cnm

foreign import ccall "cairo_tag_end" c_cairo_tag_end ::
	Ptr (CairoT s) -> CString -> IO ()
