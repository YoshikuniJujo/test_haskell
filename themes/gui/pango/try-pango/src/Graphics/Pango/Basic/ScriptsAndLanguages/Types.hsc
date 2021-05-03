{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.Types where

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.C.String
import Data.Int
import System.IO.Unsafe
import Graphics.Pango.Template

#include <pango/pango.h>

newtype PangoScript = PangoScript #{type PangoScript} deriving Show

mkMemberPangoScript :: String -> Integer -> DecsQ
mkMemberPangoScript = mkMemberGen ''PangoScript 'PangoScript

newtype PangoLanguage = PangoLanguage (Ptr PangoLanguage)

instance Show PangoLanguage where
	showsPrec d l = showParen (d > 10)
		$ ("PangoLanguage " ++) . (pangoLanguageToString l ++)

pangoLanguageToString :: PangoLanguage -> String
pangoLanguageToString (PangoLanguage pl) =
	unsafePerformIO $ peekCString =<< c_pango_language_to_string pl

foreign import ccall "pango_language_to_string"
	c_pango_language_to_string :: Ptr PangoLanguage -> IO CString
