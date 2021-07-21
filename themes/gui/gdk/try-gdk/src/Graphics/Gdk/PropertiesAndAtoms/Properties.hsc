{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.PropertiesAndAtoms.Properties where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Int

import Graphics.Gdk.Windows
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

#include <gdk/gdk.h>

gdkPropertyGet :: GdkWindow -> GdkAtom -> GdkAtom -> CULong -> CULong -> Bool ->
	IO (Maybe (GdkAtom, CInt, CInt, String))
gdkPropertyGet w p t o l b =
	alloca \pat -> alloca \paf -> alloca \pal -> alloca \pdt ->
		c_gdk_property_get w p t o l (boolToCInt b) pat paf pal pdt >>= \case
			#{const FALSE} -> pure Nothing
			#{const TRUE} -> do
				(at, af, al, cdt) <- (,,,)
					<$> peek pat <*> peek paf <*> peek pal <*> peek pdt
				dt <- peekCString cdt
				c_g_free cdt
				pure $ Just (at, af, al, dt)
			_ -> error "gdk_property_get should return FALSE or TRUE"
		

foreign import ccall "gdk_property_get"
	c_gdk_property_get ::
		GdkWindow -> GdkAtom -> GdkAtom -> CULong -> CULong -> CInt ->
		Ptr GdkAtom -> Ptr CInt -> Ptr CInt -> Ptr CString ->
		IO #{type gboolean}

boolToCInt :: Bool -> CInt
boolToCInt = \case False -> #{const FALSE}; True -> #{const TRUE}

foreign import ccall "g_free" c_g_free :: Ptr a -> IO ()
