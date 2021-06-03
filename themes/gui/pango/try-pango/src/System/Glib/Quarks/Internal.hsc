{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.Quarks.Internal (
	GQuark(..),
	gQuarkFromString, gQuarkToString, gInternString, gUninternString,
	gQuarkTryString ) where

import Foreign.Storable
import Foreign.C.String
import Data.Word

import System.IO.Unsafe

#include <gmodule.h>

newtype GQuark = GQuark #{type GQuark} deriving (Eq, Storable)

instance Show GQuark where show _ = "GQuark"

gQuarkFromString :: String -> GQuark
gQuarkFromString s =
	unsafePerformIO $ GQuark <$> withCString s c_g_quark_from_string

foreign import ccall "g_quark_from_string" c_g_quark_from_string ::
	CString -> IO #{type GQuark}

gQuarkToString :: GQuark -> String
gQuarkToString (GQuark q) =
	unsafePerformIO $ peekCString =<< c_g_quark_to_string q

foreign import ccall "g_quark_to_string" c_g_quark_to_string ::
	#{type GQuark} -> IO CString

gQuarkTryString :: String -> IO (Maybe GQuark)
gQuarkTryString s = (<$> withCString s c_g_quark_try_string) \case
		0 -> Nothing; q -> Just $ GQuark q

foreign import ccall "g_quark_try_string" c_g_quark_try_string ::
	CString -> IO #{type GQuark}

newtype InternedString = InternedString CString deriving Eq

instance Show InternedString where show _ = "InternedString"

gInternString :: String -> InternedString
gInternString s =
	unsafePerformIO $ InternedString <$> withCString s c_g_intern_string

gUninternString :: InternedString -> String
gUninternString (InternedString i) = unsafePerformIO $ peekCString i

foreign import ccall "g_intern_string" c_g_intern_string ::
	CString -> IO CString
