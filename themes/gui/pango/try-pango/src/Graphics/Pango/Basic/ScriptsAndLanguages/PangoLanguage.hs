{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage (
	-- * TYPE
	PangoLanguage, pattern PangoLanguage, getPangoLanguage,

	-- * FUNCTION
	pangoLanguageMatches, pangoLanguageIncludesScript,
	pangoLanguageGetScripts, pangoLanguageGetDefault,
	pangoLanguageGetSampleString ) where

import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage.Internal
