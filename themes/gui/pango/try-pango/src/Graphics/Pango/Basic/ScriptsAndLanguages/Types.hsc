{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.Types where

import Language.Haskell.TH
import Foreign.Ptr
import Data.Int
import Graphics.Pango.Template

#include <pango/pango.h>

newtype PangoScript = PangoScript #{type PangoScript} deriving Show

mkMemberPangoScript :: String -> Integer -> DecsQ
mkMemberPangoScript = mkMemberGen ''PangoScript 'PangoScript

newtype PangoLanguage = PangoLanguage (Ptr PangoLanguage)

instance Show PangoLanguage where show _ = "PangoLanguage"
