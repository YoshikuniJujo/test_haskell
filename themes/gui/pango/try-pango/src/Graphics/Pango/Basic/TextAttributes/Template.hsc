{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes.Template where

import Language.Haskell.TH
import Data.Word
import Graphics.Pango.Template

#include <pango/pango.h>

newtype PangoAttrType = PangoAttrType #{type PangoAttrType} deriving Show

mkMemberAttrType :: String -> Integer -> DecsQ
mkMemberAttrType = mkMemberGen ''PangoAttrType 'PangoAttrType
