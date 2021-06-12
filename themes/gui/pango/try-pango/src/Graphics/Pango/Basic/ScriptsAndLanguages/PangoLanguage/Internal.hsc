{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage.Internal (
	-- * TYPE
	PangoLanguage(..), pattern PangoLanguage, getPangoLanguage,

	-- * FUNCTION
	pangoLanguageMatches, pangoLanguageIncludesScript,
	pangoLanguageGetScripts, pangoLanguageGetDefault,
	pangoLanguageGetSampleString ) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Int
import Data.Text.CString
import Text.Read
import System.IO.Unsafe

import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript.Enum

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

newtype PangoLanguage = PangoLanguage_ (Ptr PangoLanguage)

instance Show PangoLanguage where
	showsPrec d l = showParen (d > 10)
		$ ("PangoLanguage " ++) . (show (pangoLanguageToString l) ++)

instance Read PangoLanguage where
	readPrec = parens $ prec appPrec do
		Ident "PangoLanguage" <- lexP
		s <- step readPrec
		pure $ PangoLanguage s
		where appPrec = 10

{-# COMPLETE PangoLanguage #-}

pattern PangoLanguage :: String -> PangoLanguage
pattern PangoLanguage { getPangoLanguage }
		<- (pangoLanguageToString -> getPangoLanguage) where
	PangoLanguage s = pangoLanguageFromString s

pangoLanguageToString :: PangoLanguage -> String
pangoLanguageToString (PangoLanguage_ pl) =
	unsafePerformIO $ peekCString =<< c_pango_language_to_string pl

foreign import ccall "pango_language_to_string"
	c_pango_language_to_string :: Ptr PangoLanguage -> IO CString

pangoLanguageFromString :: String -> PangoLanguage
pangoLanguageFromString l = unsafePerformIO $ withCString l \cl ->
	PangoLanguage_ <$> c_pango_language_from_string cl

foreign import ccall "pango_language_from_string"
	c_pango_language_from_string :: CString -> IO (Ptr PangoLanguage)
