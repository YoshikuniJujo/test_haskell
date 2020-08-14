{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.Internal.XK where

import Control.Moffy.Event.Key.Internal (
	xkVoidSymbol, xkTtyFunctionKeys, xkJapaneseKeyboard,
	xkCursorControlAndMotion, xkMiscFunctions, xkF1ToF35, xkModifiers,
	xkIsoLeftTab, xkSpaceToSlash, xkDigit, xkColonToAt, xkUpperAlph,
	xkBlacketleftToGrave, xkLowerAlph, xkBraceleftToAsciitilde )

---------------------------------------------------------------------------

-- + VOID SYMBOL
-- + NOT VISIBLE
-- + ASCII

---------------------------------------------------------------------------

-- * Key

---------------------------------------------------------------------------
-- VOID SYMBOL
---------------------------------------------------------------------------

-- ** Void Symbol

xkVoidSymbol

---------------------------------------------------------------------------
-- NOT VISIBLE
---------------------------------------------------------------------------

-- ** Tty Function Keys

xkTtyFunctionKeys

-- ** Japanese Keyboard

xkJapaneseKeyboard

-- ** Cursor Control & Motion

xkCursorControlAndMotion

-- ** Misc Function

xkMiscFunctions

-- ** Ausiliary Functions: F1 to F35

xkF1ToF35

-- ** Modifiers

xkModifiers

-- ** Keyboard (Xkb) Extension Function and Modifier Keys

xkIsoLeftTab
	
---------------------------------------------------------------------------
-- ASCII
---------------------------------------------------------------------------

-- ** Latin 1 (Only Ascii)

-- *** Space to Slash

xkSpaceToSlash

-- *** Digit

xkDigit

-- *** Colon to At

xkColonToAt

-- *** Upper Alphabet

xkUpperAlph

-- *** Blacketleft to Grave

xkBlacketleftToGrave

-- *** Lower Alphabet

xkLowerAlph

-- *** Braceleft to Asciitilde

xkBraceleftToAsciitilde
