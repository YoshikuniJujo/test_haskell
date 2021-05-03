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

mkMemberPangoScript "PangoScriptNewTaiLue" #{const PANGO_SCRIPT_NEW_TAI_LUE}
mkMemberPangoScript "PangoScriptBuginese" #{const PANGO_SCRIPT_BUGINESE}
mkMemberPangoScript "PangoScriptGlagolitic" #{const PANGO_SCRIPT_GLAGOLITIC}
mkMemberPangoScript "PangoScriptTifinagh" #{const PANGO_SCRIPT_TIFINAGH}
mkMemberPangoScript "PangoScriptSylotiNagri" #{const PANGO_SCRIPT_SYLOTI_NAGRI}
mkMemberPangoScript "PangoScriptOldPersian" #{const PANGO_SCRIPT_OLD_PERSIAN}
mkMemberPangoScript "PangoScriptKharoshthi" #{const PANGO_SCRIPT_KHAROSHTHI}

mkMemberPangoScript "PangoScriptUnknown" #{const PANGO_SCRIPT_UNKNOWN}
mkMemberPangoScript "PangoScriptBalinese" #{const PANGO_SCRIPT_BALINESE}
mkMemberPangoScript "PangoScriptCuneiform" #{const PANGO_SCRIPT_CUNEIFORM}
mkMemberPangoScript "PangoScriptPhoenician" #{const PANGO_SCRIPT_PHOENICIAN}
mkMemberPangoScript "PangoScriptPhagsPa" #{const PANGO_SCRIPT_PHAGS_PA}
mkMemberPangoScript "PangoScriptNko" #{const PANGO_SCRIPT_NKO}

mkMemberPangoScript "PangoScriptKyahLi" #{const PANGO_SCRIPT_KAYAH_LI}
mkMemberPangoScript "PangoScriptLepcha" #{const PANGO_SCRIPT_LEPCHA}
mkMemberPangoScript "PangoScriptRejang" #{const PANGO_SCRIPT_REJANG}
mkMemberPangoScript "PangoScriptSundanese" #{const PANGO_SCRIPT_SUNDANESE}
mkMemberPangoScript "PangoScriptSaurashtra" #{const PANGO_SCRIPT_SAURASHTRA}
mkMemberPangoScript "PangoScriptCham" #{const PANGO_SCRIPT_CHAM}
mkMemberPangoScript "PangoScriptOlChiki" #{const PANGO_SCRIPT_OL_CHIKI}
mkMemberPangoScript "PangoScriptVai" #{const PANGO_SCRIPT_VAI}
mkMemberPangoScript "PangoScriptCarian" #{const PANGO_SCRIPT_CARIAN}
mkMemberPangoScript "PangoScriptLycian" #{const PANGO_SCRIPT_LYCIAN}
mkMemberPangoScript "PangoScriptLydian" #{const PANGO_SCRIPT_LYDIAN}

mkMemberPangoScript "PangoScriptBatak" #{const PANGO_SCRIPT_BATAK}
mkMemberPangoScript "PangoScriptBraham" #{const PANGO_SCRIPT_BRAHMI}
mkMemberPangoScript "PangoScriptMandaic" #{const PANGO_SCRIPT_MANDAIC}
mkMemberPangoScript "PangoScriptChakma" #{const PANGO_SCRIPT_CHAKMA}
mkMemberPangoScript "PangoScriptMeroiticCursive"
	#{const PANGO_SCRIPT_MEROITIC_CURSIVE}
mkMemberPangoScript "PangoScriptMeroiticHieroglyphs"
	#{const PANGO_SCRIPT_MEROITIC_HIEROGLYPHS}
mkMemberPangoScript "PangoScriptMiao" #{const PANGO_SCRIPT_MIAO}
mkMemberPangoScript "PangoScriptSharada" #{const PANGO_SCRIPT_SHARADA}
mkMemberPangoScript "PangoScriptSoraSompeng" #{const PANGO_SCRIPT_SORA_SOMPENG}
mkMemberPangoScript "PangoScriptTakri" #{const PANGO_SCRIPT_TAKRI}

mkMemberPangoScript "PangoScriptBassaVah" #{const PANGO_SCRIPT_BASSA_VAH}
mkMemberPangoScript "PangoScriptCaucasianAlbanian"
	#{const PANGO_SCRIPT_CAUCASIAN_ALBANIAN}
mkMemberPangoScript "PangoScriptDuployan" #{const PANGO_SCRIPT_DUPLOYAN}
mkMemberPangoScript "PangoScriptElbasan" #{const PANGO_SCRIPT_ELBASAN}
mkMemberPangoScript "PangoScriptGrantha" #{const PANGO_SCRIPT_GRANTHA}
mkMemberPangoScript "PangoScriptKhojki" #{const PANGO_SCRIPT_KHOJKI}
mkMemberPangoScript "PangoScriptKhudawadi" #{const PANGO_SCRIPT_KHUDAWADI}
mkMemberPangoScript "PangoScriptLinearA" #{const PANGO_SCRIPT_LINEAR_A}
mkMemberPangoScript "PangoScriptMahajani" #{const PANGO_SCRIPT_MAHAJANI}
mkMemberPangoScript "PangoScriptManichaean" #{const PANGO_SCRIPT_MANICHAEAN}
mkMemberPangoScript "PangoScriptMendeKikakui"
	#{const PANGO_SCRIPT_MENDE_KIKAKUI}
mkMemberPangoScript "PangoScriptModi" #{const PANGO_SCRIPT_MODI}
mkMemberPangoScript "PangoScriptMro" #{const PANGO_SCRIPT_MRO}
mkMemberPangoScript "PangoScriptNabataean" #{const PANGO_SCRIPT_NABATAEAN}
mkMemberPangoScript "PangoScriptOldNorthArabian" #{const PANGO_SCRIPT_OLD_NORTH_ARABIAN}
mkMemberPangoScript "PangoScriptOldPermic" #{const PANGO_SCRIPT_OLD_PERMIC}
mkMemberPangoScript "PangoScriptPahawhHmong" #{const PANGO_SCRIPT_PAHAWH_HMONG}
mkMemberPangoScript "PangoScriptPalmyrene" #{const PANGO_SCRIPT_PALMYRENE}
mkMemberPangoScript "PangoScriptPauCinHau" #{const PANGO_SCRIPT_PAU_CIN_HAU}
mkMemberPangoScript "PangoScriptPsalterPahlavi" #{const PANGO_SCRIPT_PSALTER_PAHLAVI}
mkMemberPangoScript "PangoScriptSiddham" #{const PANGO_SCRIPT_SIDDHAM}
mkMemberPangoScript "PangoScriptTirhuta" #{const PANGO_SCRIPT_TIRHUTA}
mkMemberPangoScript "PangoScriptWarangCiti" #{const PANGO_SCRIPT_WARANG_CITI}
mkMemberPangoScript "PangoScriptAhom" #{const PANGO_SCRIPT_AHOM}
mkMemberPangoScript "PangoScriptAnatolianHieroglyphs"
	#{const PANGO_SCRIPT_ANATOLIAN_HIEROGLYPHS}
mkMemberPangoScript "PangoScriptHatran" #{const PANGO_SCRIPT_HATRAN}
mkMemberPangoScript "PangoScriptMultani" #{const PANGO_SCRIPT_MULTANI}
mkMemberPangoScript "PangoScriptOldHungarian" #{const PANGO_SCRIPT_OLD_HUNGARIAN}
mkMemberPangoScript "PangoScriptSignwriting" #{const PANGO_SCRIPT_SIGNWRITING}
