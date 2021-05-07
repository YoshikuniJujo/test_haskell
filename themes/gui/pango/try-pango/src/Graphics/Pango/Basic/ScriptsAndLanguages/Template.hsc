{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.Template (
	PangoScript(..), mkMemberPangoScript ) where

import Language.Haskell.TH
import Data.Int
import Graphics.Pango.Template

#include <pango/pango.h>

newtype PangoScript = PangoScript #{type PangoScript} deriving Show


mkMemberPangoScript :: String -> Integer -> DecsQ
mkMemberPangoScript = mkMemberGen ''PangoScript 'PangoScript
