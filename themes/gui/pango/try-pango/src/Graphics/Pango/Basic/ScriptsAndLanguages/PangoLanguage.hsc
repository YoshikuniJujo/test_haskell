{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage where

import Foreign.Ptr
import Foreign.C.String
import System.IO.Unsafe

#include <pango/pango.h>

newtype PangoLanguage = PangoLanguage (Ptr PangoLanguage)

instance Show PangoLanguage where show _ = "PangoLanguage"

pangoLanguageFromString :: String -> PangoLanguage
pangoLanguageFromString l = unsafePerformIO $ withCString l \cl ->
	PangoLanguage <$> c_pango_language_from_string cl


foreign import ccall "pango_language_from_string"
	c_pango_language_from_string :: CString -> IO (Ptr PangoLanguage)

pangoLanguageToString :: PangoLanguage -> String
pangoLanguageToString (PangoLanguage pl) =
	unsafePerformIO $ peekCString =<< c_pango_language_to_string pl

foreign import ccall "pango_language_to_string"
	c_pango_language_to_string :: Ptr PangoLanguage -> IO CString

pangoLanguageGetDefault :: IO PangoLanguage
pangoLanguageGetDefault = PangoLanguage <$> c_pango_language_get_default

foreign import ccall "pango_language_get_default"
	c_pango_language_get_default :: IO (Ptr PangoLanguage)

pangoLanguageGetSampleString :: PangoLanguage -> String
pangoLanguageGetSampleString (PangoLanguage pl) = unsafePerformIO
	$ peekCString =<< c_pango_language_get_sample_string pl

foreign import ccall "pango_language_get_sample_string"
	c_pango_language_get_sample_string :: Ptr PangoLanguage -> IO CString
