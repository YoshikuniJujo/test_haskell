{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Int
import Data.Text.CString
import System.IO.Unsafe

import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript.Enum
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguageType

import qualified Data.Text as T

#include <pango/pango.h>

pangoLanguageGetDefault :: IO PangoLanguage
pangoLanguageGetDefault = PangoLanguage_ <$> c_pango_language_get_default

foreign import ccall "pango_language_get_default"
	c_pango_language_get_default :: IO (Ptr PangoLanguage)

pangoLanguageGetSampleString :: PangoLanguage -> T.Text
pangoLanguageGetSampleString (PangoLanguage_ pl) = unsafePerformIO
	$ peekCStringText =<< c_pango_language_get_sample_string pl

foreign import ccall "pango_language_get_sample_string"
	c_pango_language_get_sample_string :: Ptr PangoLanguage -> IO CString

pangoLanguageMatches :: PangoLanguage -> String -> Bool
pangoLanguageMatches (PangoLanguage_ l) rl = unsafePerformIO
	$ withCString rl \crl -> (<$> c_pango_language_matches l crl) \case
		#{const FALSE} -> False; #{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_language_matches" c_pango_language_matches ::
	Ptr PangoLanguage -> CString -> IO #{type gboolean}

pangoLanguageIncludesScript :: PangoLanguage -> PangoScript -> Bool
pangoLanguageIncludesScript (PangoLanguage_ l) (PangoScript s) = unsafePerformIO
	$ (<$> c_pango_language_includes_script l s) \case
		#{const FALSE} -> False; #{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_language_includes_script" c_pango_language_includes_script ::
	Ptr PangoLanguage -> #{type PangoScript} -> IO #{type gboolean}

pangoLanguageGetScripts :: PangoLanguage -> [PangoScript]
pangoLanguageGetScripts (PangoLanguage_ l) = unsafePerformIO
	$ (PangoScript <$>) <$> alloca \pn -> do
		ss <- c_pango_language_get_scripts l pn
		n <- peek pn
		peekArray (fromIntegral n) ss

foreign import ccall "pango_language_get_scripts" c_pango_language_get_scripts ::
	Ptr PangoLanguage -> Ptr CInt -> IO (Ptr #{type PangoScript})
