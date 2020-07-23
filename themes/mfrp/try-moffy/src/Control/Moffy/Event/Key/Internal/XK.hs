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

-- * VOID SYMBOL
-- * NOT VISIBLE
-- * ASCII

---------------------------------------------------------------------------
-- VOID SYMBOL
---------------------------------------------------------------------------

xkVoidSymbol

---------------------------------------------------------------------------
-- NOT VISIBLE
---------------------------------------------------------------------------

xkTtyFunctionKeys
xkJapaneseKeyboard
xkCursorControlAndMotion
xkMiscFunctions
xkF1ToF35
xkModifiers
xkIsoLeftTab
	
---------------------------------------------------------------------------
-- ASCII
---------------------------------------------------------------------------

xkSpaceToSlash
xkDigit
xkColonToAt
xkUpperAlph
xkBlacketleftToGrave
xkLowerAlph
xkBraceleftToAsciitilde
