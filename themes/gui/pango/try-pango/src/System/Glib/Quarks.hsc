{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.Quarks where

import Foreign.Storable
import Foreign.C.String
import Data.Word

#include <gmodule.h>

newtype GQuark = GQuark #{type GQuark} deriving (Show, Eq, Storable)

gQuarkFromString :: String -> IO GQuark
gQuarkFromString s = GQuark <$> withCString s c_g_quark_from_string

foreign import ccall "g_quark_from_string" c_g_quark_from_string ::
	CString -> IO #{type GQuark}

gQuarkToString :: GQuark -> IO String
gQuarkToString (GQuark q) = peekCString =<< c_g_quark_to_string q

foreign import ccall "g_quark_to_string" c_g_quark_to_string ::
	#{type GQuark} -> IO CString

gQuarkTryString :: String -> IO (Maybe GQuark)
gQuarkTryString s = (<$> withCString s c_g_quark_try_string) \case
		0 -> Nothing; q -> Just $ GQuark q

foreign import ccall "g_quark_try_string" c_g_quark_try_string ::
	CString -> IO #{type GQuark}

newtype InternedString = InternedString CString deriving (Show, Eq)

gInternString :: String -> IO InternedString
gInternString s = InternedString <$> withCString s c_g_intern_string

foreign import ccall "g_intern_string" c_g_intern_string ::
	CString -> IO CString
