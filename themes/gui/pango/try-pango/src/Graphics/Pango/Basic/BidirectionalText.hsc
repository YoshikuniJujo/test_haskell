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
