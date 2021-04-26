{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayout.Template where

import Language.Haskell.TH
import Data.Word
import Graphics.Pango.Template

#include <pango/pango.h>

newtype PangoWrapMode = PangoWrapMode #{type PangoWrapMode} deriving Show

mkMemberPangoWrapMode :: String -> Integer -> DecsQ
mkMemberPangoWrapMode = mkMemberGen ''PangoWrapMode 'PangoWrapMode
