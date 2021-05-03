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
mkMemberPangoScript "PangoScriptInherited" #{const PANGO_SCRIPT_INHERITED}

mkMemberPangoScript "PangoScriptArabic" #{const PANGO_SCRIPT_ARABIC}
mkMemberPangoScript "PangoScriptArmenian" #{const PANGO_SCRIPT_ARMENIAN}
mkMemberPangoScript "PangoScriptBengali" #{const PANGO_SCRIPT_BENGALI}
mkMemberPangoScript "PangoScriptBopomofo" #{const PANGO_SCRIPT_BOPOMOFO}
mkMemberPangoScript "PangoScriptCherokee" #{const PANGO_SCRIPT_CHEROKEE}
mkMemberPangoScript "PangoScriptCoptic" #{const PANGO_SCRIPT_COPTIC}
mkMemberPangoScript "PangoScriptCyrillic" #{const PANGO_SCRIPT_CYRILLIC}
mkMemberPangoScript "PangoScriptDeseret" #{const PANGO_SCRIPT_DESERET}
mkMemberPangoScript "PangoScriptDevanagari" #{const PANGO_SCRIPT_DEVANAGARI}
mkMemberPangoScript "PangoScriptEthiopic" #{const PANGO_SCRIPT_ETHIOPIC}
mkMemberPangoScript "PangoScriptGeorgian" #{const PANGO_SCRIPT_GEORGIAN}
mkMemberPangoScript "PangoScriptGothic" #{const PANGO_SCRIPT_GOTHIC}
mkMemberPangoScript "PangoScriptGreek" #{const PANGO_SCRIPT_GREEK}
mkMemberPangoScript "PangoScriptGujarati" #{const PANGO_SCRIPT_GUJARATI}
mkMemberPangoScript "PangoScriptGurmukhi" #{const PANGO_SCRIPT_GURMUKHI}
mkMemberPangoScript "PangoScriptHan" #{const PANGO_SCRIPT_HAN}
mkMemberPangoScript "PangoScriptHangul" #{const PANGO_SCRIPT_HANGUL}
mkMemberPangoScript "PangoScriptHebrew" #{const PANGO_SCRIPT_HEBREW}
mkMemberPangoScript "PangoScriptHiragana" #{const PANGO_SCRIPT_HIRAGANA}
mkMemberPangoScript "PangoScriptKannada" #{const PANGO_SCRIPT_KANNADA}
mkMemberPangoScript "PangoScriptKatakana" #{const PANGO_SCRIPT_KATAKANA}
mkMemberPangoScript "PangoScriptKhmer" #{const PANGO_SCRIPT_KHMER}
mkMemberPangoScript "PangoScriptLao" #{const PANGO_SCRIPT_LAO}
mkMemberPangoScript "PangoScriptLatin" #{const PANGO_SCRIPT_LATIN}
mkMemberPangoScript "PangoScriptMalaylam" #{const PANGO_SCRIPT_MALAYALAM}
mkMemberPangoScript "PangoScriptMongolian" #{const PANGO_SCRIPT_MONGOLIAN}
mkMemberPangoScript "PangoScriptMyanmar" #{const PANGO_SCRIPT_MYANMAR}
mkMemberPangoScript "PangoScriptOgham" #{const PANGO_SCRIPT_OGHAM}
mkMemberPangoScript "PangoScriptOldItalic" #{const PANGO_SCRIPT_OLD_ITALIC}
mkMemberPangoScript "PangoScriptOriya" #{const PANGO_SCRIPT_ORIYA}
mkMemberPangoScript "PangoScriptRunic" #{const PANGO_SCRIPT_RUNIC}
mkMemberPangoScript "PangoScriptSinhala" #{const PANGO_SCRIPT_SINHALA}
mkMemberPangoScript "PangoScriptSyriac" #{const PANGO_SCRIPT_SYRIAC}
mkMemberPangoScript "PangoScriptTamil" #{const PANGO_SCRIPT_TAMIL}
mkMemberPangoScript "PangoScriptTelugu" #{const PANGO_SCRIPT_TELUGU}
mkMemberPangoScript "PangoScriptThaana" #{const PANGO_SCRIPT_THAANA}
mkMemberPangoScript "PangoScriptThai" #{const PANGO_SCRIPT_THAI}
mkMemberPangoScript "PangoScriptTibetan" #{const PANGO_SCRIPT_TIBETAN}
mkMemberPangoScript "PangoScriptCanadianAboriginal"
	#{const PANGO_SCRIPT_CANADIAN_ABORIGINAL}
mkMemberPangoScript "PangoScriptYi" #{const PANGO_SCRIPT_YI}
mkMemberPangoScript "PangoScriptTagalog" #{const PANGO_SCRIPT_TAGALOG}
mkMemberPangoScript "PangoScriptHanunoo" #{const PANGO_SCRIPT_HANUNOO}
mkMemberPangoScript "PangoScriptBuhid" #{const PANGO_SCRIPT_BUHID}
mkMemberPangoScript "PangoScriptTagbanwa" #{const PANGO_SCRIPT_TAGBANWA}
mkMemberPangoScript "PangoScriptBraille" #{const PANGO_SCRIPT_BRAILLE}
mkMemberPangoScript "PangoScriptCypriot" #{const PANGO_SCRIPT_CYPRIOT}
mkMemberPangoScript "PangoScriptLimbu" #{const PANGO_SCRIPT_LIMBU}
mkMemberPangoScript "PangoScriptOsmanya" #{const PANGO_SCRIPT_OSMANYA}
mkMemberPangoScript "PangoscriptShavian" #{const PANGO_SCRIPT_SHAVIAN}
mkMemberPangoScript "PangoScriptLinearB" #{const PANGO_SCRIPT_LINEAR_B}
mkMemberPangoScript "PangoScriptTaiLe" #{const PANGO_SCRIPT_TAI_LE}
mkMemberPangoScript "PangoScriptUgaritic" #{const PANGO_SCRIPT_UGARITIC}
