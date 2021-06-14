{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.BidirectionalText (
	-- * ENUM
	-- ** PangoDirection
	PangoDirection,
	pattern PangoDirectionLtr, pattern PangoDirectionRtl,
	pattern PangoDirectionTtbLtr, pattern PangoDirectionTtbRtl,
	pattern PangoDirectionWeakLtr, pattern PangoDirectionWeakRtl,
	pattern PangoDirectionNeutral,

	-- ** PangoBidiType
	PangoBidiType,
	pattern PangoBidiTypeL, pattern PangoBidiTypeLre,
	pattern PangoBidiTypeLro, pattern PangoBidiTypeR,
	pattern PangoBidiTypeAl, pattern PangoBidiTypeRle,
	pattern PangoBidiTypeRlo, pattern PangoBidiTypePdf,
	pattern PangoBidiTypeEn, pattern PangoBidiTypeEs,
	pattern PangoBidiTypeEt, pattern PangoBidiTypeAn,
	pattern PangoBidiTypeCs, pattern PangoBidiTypeNsm,
	pattern PangoBidiTypeBn, pattern PangoBidiTypeB,
	pattern PangoBidiTypeS, pattern PangoBidiTypeWs,
	pattern PangoBidiTypeOn,

	-- * FUNCTION
	pangoUnicharDirection, pangoFindBaseDir, pangoBidiTypeForUnichar ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Data.Word
import Data.Char
import System.IO.Unsafe

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

#include <pango/pango.h>

enum "PangoDirection" ''#{type PangoDirection} [''Show, ''Read] [
	("PangoDirectionLtr", #{const PANGO_DIRECTION_LTR}),
	("PangoDirectionRtl", #{const PANGO_DIRECTION_RTL}),
	("PangoDirectionTtbLtr", #{const PANGO_DIRECTION_TTB_LTR}),
	("PangoDirectionTtbRtl", #{const PANGO_DIRECTION_TTB_RTL}),
	("PangoDirectionWeakLtr", #{const PANGO_DIRECTION_WEAK_LTR}),
	("PangoDirectionWeakRtl", #{const PANGO_DIRECTION_WEAK_RTL}),
	("PangoDirectionNeutral", #{const PANGO_DIRECTION_NEUTRAL}) ]

enum "PangoBidiType" ''#{type PangoBidiType} [''Show, ''Read] [
	("PangoBidiTypeL", #{const PANGO_BIDI_TYPE_L}),
	("PangoBidiTypeLre", #{const PANGO_BIDI_TYPE_LRE}),
	("PangoBidiTypeLro", #{const PANGO_BIDI_TYPE_LRO}),
	("PangoBidiTypeR", #{const PANGO_BIDI_TYPE_R}),
	("PangoBidiTypeAl", #{const PANGO_BIDI_TYPE_AL}),
	("PangoBidiTypeRle", #{const PANGO_BIDI_TYPE_RLE}),
	("PangoBidiTypeRlo", #{const PANGO_BIDI_TYPE_RLO}),
	("PangoBidiTypePdf", #{const PANGO_BIDI_TYPE_PDF}),
	("PangoBidiTypeEn", #{const PANGO_BIDI_TYPE_EN}),
	("PangoBidiTypeEs", #{const PANGO_BIDI_TYPE_ES}),
	("PangoBidiTypeEt", #{const PANGO_BIDI_TYPE_ET}),
	("PangoBidiTypeAn", #{const PANGO_BIDI_TYPE_AN}),
	("PangoBidiTypeCs", #{const PANGO_BIDI_TYPE_CS}),
	("PangoBidiTypeNsm", #{const PANGO_BIDI_TYPE_NSM}),
	("PangoBidiTypeBn", #{const PANGO_BIDI_TYPE_BN}),
	("PangoBidiTypeB", #{const PANGO_BIDI_TYPE_B}),
	("PangoBidiTypeS", #{const PANGO_BIDI_TYPE_S}),
	("PangoBidiTypeWs", #{const PANGO_BIDI_TYPE_WS}),
	("PangoBidiTypeOn", #{const PANGO_BIDI_TYPE_ON}) ]

pangoUnicharDirection :: Char -> PangoDirection
pangoUnicharDirection =
	unsafePerformIO . c_pango_unichar_direction . fromIntegral . ord

foreign import ccall "pango_unichar_direction" c_pango_unichar_direction ::
	#{type gunichar} -> IO PangoDirection

pangoFindBaseDir :: T.Text -> PangoDirection
pangoFindBaseDir t = unsafePerformIO
	$ T.withCStringLen t \(cs, l) -> c_pango_find_base_dir cs $ fromIntegral l

foreign import ccall "pango_find_base_dir" c_pango_find_base_dir ::
	CString -> CInt -> IO PangoDirection

pangoBidiTypeForUnichar :: Char -> PangoBidiType
pangoBidiTypeForUnichar =
	unsafePerformIO . c_pango_bidi_type_for_unichar . fromIntegral . ord

foreign import ccall "pango_bidi_type_for_unichar"
	c_pango_bidi_type_for_unichar :: #{type gunichar} -> IO PangoBidiType
