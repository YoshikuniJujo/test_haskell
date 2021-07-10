{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkModifierType where

import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word

#include <gdk/gdk.h>

enum "GdkModifierTypeSingleBit" ''#{type GdkModifierType} [''Show] [
	("GdkShiftMask", #{const GDK_SHIFT_MASK}),
	("GdkLockMask", #{const GDK_LOCK_MASK}),
	("GdkControlMask", #{const GDK_CONTROL_MASK}),
	("GdkMod1Mask", #{const GDK_MOD1_MASK}),
	("GdkMod2Mask", #{const GDK_MOD2_MASK}),
	("GdkMod3Mask", #{const GDK_MOD3_MASK}),
	("GdkMod4Mask", #{const GDK_MOD4_MASK}),
	("GdkMod5Mask", #{const GDK_MOD5_MASK}),
	("GdkButton1Mask", #{const GDK_BUTTON1_MASK}),
	("GdkButton2Mask", #{const GDK_BUTTON2_MASK}),
	("GdkButton3Mask", #{const GDK_BUTTON3_MASK}),
	("GdkButton4Mask", #{const GDK_BUTTON4_MASK}),
	("GdkButton5Mask", #{const GDK_BUTTON5_MASK}),
	("GdkSuperMask", #{const GDK_SUPER_MASK}),
	("GdkHyperMask", #{const GDK_HYPER_MASK}),
	("GdkMetaMask", #{const GDK_META_MASK}) ]

enum "GdkModifierTypeMultiBits" ''#{type GdkModifierType} [''Show] [
	("GdkNoModifierMask", 0),
	("GdkModifierMask", #{const GDK_MODIFIER_MASK}) ]

gdkModifierTypeMultiBits ::
	[GdkModifierTypeSingleBit] -> GdkModifierTypeMultiBits
gdkModifierTypeMultiBits = GdkModifierTypeMultiBits
	. foldr ((.|.) . (\(GdkModifierTypeSingleBit mt) -> mt)) 0

gdkModifierTypeSingleBitList ::
	GdkModifierTypeMultiBits -> [GdkModifierTypeSingleBit]
gdkModifierTypeSingleBitList (GdkModifierTypeMultiBits mts) =
	GdkModifierTypeSingleBit
		<$> separateBits (#{size GdkModifierType} * 8) mts
