{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.Template where

import Language.Haskell.TH
import Data.Int
import Graphics.Pango.Template

#include <pango/pango.h>

newtype PangoScript = PangoScript #{type PangoScript} deriving Show

mkMemberPangoScript :: String -> Integer -> DecsQ
mkMemberPangoScript = mkMemberGen ''PangoScript 'PangoScript
