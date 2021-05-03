{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage where

import Foreign.Ptr
import Foreign.C.String
import System.IO.Unsafe

import Graphics.Pango.Basic.ScriptsAndLanguages.Types

#include <pango/pango.h>

pangoLanguageGetDefault :: IO PangoLanguage
pangoLanguageGetDefault = PangoLanguage_ <$> c_pango_language_get_default

foreign import ccall "pango_language_get_default"
	c_pango_language_get_default :: IO (Ptr PangoLanguage)

pangoLanguageGetSampleString :: PangoLanguage -> String
pangoLanguageGetSampleString (PangoLanguage_ pl) = unsafePerformIO
	$ peekCString =<< c_pango_language_get_sample_string pl

foreign import ccall "pango_language_get_sample_string"
	c_pango_language_get_sample_string :: Ptr PangoLanguage -> IO CString
