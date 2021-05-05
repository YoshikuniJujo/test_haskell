{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.Types where

import GHC.Read
import Language.Haskell.TH
import Foreign.Ptr
import Foreign.C.String
import Data.Int
import Text.Read
import System.IO.Unsafe
import Graphics.Pango.Template

#include <pango/pango.h>

newtype PangoScript = PangoScript #{type PangoScript} deriving Show

mkMemberPangoScript :: String -> Integer -> DecsQ
mkMemberPangoScript = mkMemberGen ''PangoScript 'PangoScript

newtype PangoLanguage = PangoLanguage_ (Ptr PangoLanguage)

instance Show PangoLanguage where
	showsPrec d l = showParen (d > 10)
		$ ("PangoLanguage " ++) . (pangoLanguageToString l ++)

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
