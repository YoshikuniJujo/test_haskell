{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.General (
	-- * INIT, DISPLAY ARGUMENT NAME, BACKENDS AND PROGRAM CLASS
	gdkInit, gdkGetDisplayArgName,
	gdkSetAllowedBackends,
	gdkGetProgramClass, gdkSetProgramClass,

	-- * GDK GRAB STATUS
	GdkGrabStatus(..),
	pattern GdkGrabSuccess, pattern GdkGrabAlreadyGrabbed,
	pattern GdkGrabInvalidTime, pattern GdkGrabNotViewable,
	pattern GdkGrabFrozen, pattern GdkGrabFailed ) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Foreign.C.Enum
import Data.Bool
import Data.Word
import Data.Int
import System.GLib.Bool

import Graphics.Gdk.Exception

#include <gdk/gdk.h>

foreign import ccall "gdk_init_check" c_gdk_init_check :: Ptr CInt -> Ptr (Ptr CString) -> IO #type gboolean

gdkInit :: String ->[String] -> IO (String, [String])
gdkInit prn as = (\(prn' : as') -> (prn', as')) <$> allocaArray (length (prn : as)) \arr -> withCStrings (prn : as) \cas -> do
	pokeArray arr cas
	(n', arr') <- alloca \pn -> do
		poke pn . fromIntegral $ length (prn : as)
		arr' <- alloca \parr -> do
			poke parr arr
			gbooleanToBool <$> c_gdk_init_check pn parr >>= bool gdkInitFail (pure ())
			peek parr
		(, arr') <$> peek pn
	(peekCString `mapM`) =<< peekArray (fromIntegral n') arr'

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings [] f = f []
withCStrings (s : ss) f = withCString s \cs -> withCStrings ss \css -> f $ cs : css

foreign import ccall "gdk_get_display_arg_name" c_gdk_get_display_arg_name :: IO CString

gdkGetDisplayArgName :: IO (Maybe String)
gdkGetDisplayArgName = c_gdk_get_display_arg_name >>= \case
	p	| p == nullPtr -> pure Nothing
		| otherwise -> Just <$> peekCString p

foreign import ccall "gdk_set_allowed_backends" c_gdk_set_allowed_backends ::
	CString -> IO ()

gdkSetAllowedBackends :: String -> IO ()
gdkSetAllowedBackends be = withCString be c_gdk_set_allowed_backends

foreign import ccall "gdk_get_program_class" c_gdk_get_program_class :: IO CString

gdkGetProgramClass :: IO (Maybe String)
gdkGetProgramClass = c_gdk_get_program_class >>= \case
	p	| p == nullPtr -> pure Nothing
		| otherwise -> Just <$> peekCString p

foreign import ccall "gdk_set_program_class" c_gdk_set_program_class :: CString -> IO ()

gdkSetProgramClass :: String -> IO ()
gdkSetProgramClass c = withCString c c_gdk_set_program_class

enum "GdkGrabStatus" ''#{type GdkGrabStatus} [''Show] [
	("GdkGrabSuccess", #{const GDK_GRAB_SUCCESS}),
	("GdkGrabAlreadyGrabbed", #{const GDK_GRAB_ALREADY_GRABBED}),
	("GdkGrabInvalidTime", #{const GDK_GRAB_INVALID_TIME}),
	("GdkGrabNotViewable", #{const GDK_GRAB_NOT_VIEWABLE}),
	("GdkGrabFrozen", #{const GDK_GRAB_FROZEN}),
	("GdkGrabFailed", #{const GDK_GRAB_FAILED}) ]
