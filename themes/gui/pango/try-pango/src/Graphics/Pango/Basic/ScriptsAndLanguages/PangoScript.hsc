{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript where

import Data.Word
import Data.Int
import Data.Char
import System.IO.Unsafe
import Graphics.Pango.Basic.ScriptsAndLanguages.Template

#include <pango/pango.h>

pangoScriptForUnichar :: Char -> PangoScript
pangoScriptForUnichar c = unsafePerformIO
	$ PangoScript <$> c_pango_script_for_unichar (fromIntegral $ ord c)

foreign import ccall "pango_script_for_unichar" c_pango_script_for_unichar ::
	#{type gunichar} -> IO #{type PangoScript}

mkMemberPangoScript "PangoScriptInvaludCode" (#{const PANGO_SCRIPT_INVALID_CODE})
mkMemberPangoScript "PangoScriptCommon" #{const PANGO_SCRIPT_COMMON}

mkMemberPangoScript "PangoScriptHan" #{const PANGO_SCRIPT_HAN}
