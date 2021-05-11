{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.BidirectionalText where

import Foreign.C.Enum
import Data.Word

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
