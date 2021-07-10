{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkModifierType where

import Foreign.C.Enum
import Data.Word

#include <gdk/gdk.h>

enum "GdkModifierTypeSingleBit" ''#{type GdkModifierType} [''Show] [
	("GdkShiftMask", #{const GDK_SHIFT_MASK}),
	("GdkLockMask", #{const GDK_LOCK_MASK}),
	("GdkControlMask", #{const GDK_CONTROL_MASK})
	]

enum "GdkModifierTypeMultiBits" ''#{type GdkModifierType} [''Show] [
	("GdkNoModifierMask", 0),
	("GdkModifierMask", #{const GDK_MODIFIER_MASK}) ]
