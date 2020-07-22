{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.Internal (
	-- * Type
	Key(..),
	-- * Template
	-- ** Void Symbol
	xkVoidSymbol,
	-- ** TTY function keys
	xkBackSpaceToDelete,
	-- ** Japanese keyboard
	xkJapaneseKeyboard,
	-- ** Cursor control & motion
	xkCursorControlAndMotion,
	-- ** Misc function
	xkMiscFunctions,
	-- ** Auxiliary functions: F1 to F35
	xkF1ToF35,
	-- ** Modifiers
	xkModifiers,
	-- ** Keyboard (XKB) Extension function and modifier keys
	xkIsoLeftTab,
	-- ** Latin 1 (only ASCII)
	xkSpaceToSlash, xkDigit,
	xkColonToAt, xkUpperAlph,
	xkBlacketleftToGrave, xkLowerAlph, xkBraceleftToAsciitilde
	) where

import Language.Haskell.TH
import Control.Monad
import Data.Word

newtype Key = Key Word32 deriving (Show, Eq, Ord)

concatR :: [[a]] -> [a]
concatR = concat . reverse

xkVoidSymbol :: DecsQ
xkVoidSymbol = mkXk "VoidSymbol" 0xffffff

xkBackSpaceToDelete :: DecsQ
xkBackSpaceToDelete = concatR <$> zipWithM (flip mkXk)
	([0xff08 .. 0xff0b] ++ [0xff0d] ++ [0xff13 .. 0xff15] ++ [0xff1b] ++ [0xffff]) [
		"BackSpace", "Tab", "Linefeed", "Clear", "Return", "Pause", "Scroll_Lock",
		"Sys_Req", "Escape", "Delete" ]

xkJapaneseKeyboard :: DecsQ
xkJapaneseKeyboard = concatR <$> zipWithM (flip mkXk) ([0xff21 .. 0xff30] ++ [0xff37, 0xff3e, 0xff3e]) [
	"Kanji", "Muhenkan", "Henkan_Mode", "Henkan", "Romaji", "Hiragana", "Katakana", "Hiragana_Katakana",
	"Zenkaku", "Hankaku", "Zenkaku_Hankaku", "Touroku", "Massho", "Kana_Lock", "Kana_Shift", "Eisu_Shift",
	"Kanji_Bangou", "Zen_Koho", "Mae_Koho" ]

xkCursorControlAndMotion :: DecsQ
xkCursorControlAndMotion = concatR <$> zipWithM (flip mkXk) [0xff50 .. 0xff58] [
	"Home", "Left", "Up", "Right", "Down", "Prior", "Next", "End", "Begin"
	]

xkMiscFunctions :: DecsQ
xkMiscFunctions = concatR <$> zipWithM (flip mkXk) ([0xff60 .. 0xff6b] ++ [0xff7e, 0xff7f]) [
	"Select", "Print", "Excecute", "Insert", "Undo", "Redo", "Menu", "Find", "Cancel",
	"Help", "Break", "Mode_switch", "Num_Lock" ]

xkF1ToF35 :: DecsQ
xkF1ToF35 = concatR <$> zipWithM mkXk (("F" ++) . show <$> [1 :: Int .. 35]) [0xffbe .. 0xffe0]

xkSpaceToSlash :: DecsQ
xkSpaceToSlash = concatR <$> zipWithM (flip mkXk) [0x0020 ..] [
	"space", "exclam", "quotedbl", "numbersign", "dollar", "percent",
	"ampersand", "apostrophe", "parenleft", "parenright", "asterisk",
	"plus", "comma", "minus", "period", "slash" ]

xkDigit :: DecsQ
xkDigit = concatR <$> zipWithM mkXk ((: []) <$> ['0' .. '9']) [0x0030 ..]

xkColonToAt :: DecsQ
xkColonToAt = concatR <$> zipWithM (flip mkXk) [0x003a ..] [
	"colon", "semicolon", "less", "equal", "greater", "question", "at" ]

xkUpperAlph :: DecsQ
xkUpperAlph = concatR <$> zipWithM mkXk ((: []) <$> ['A' .. 'Z']) [0x0041 ..]

xkBlacketleftToGrave :: DecsQ
xkBlacketleftToGrave = concatR <$> zipWithM (flip mkXk) [0x005b ..] [
	"bracketleft", "backslash", "bracketright", "asciicircum", "underscore",
	"grave" ]

xkLowerAlph :: DecsQ
xkLowerAlph = concatR <$> zipWithM mkXk ((: []) <$> ['a' .. 'z']) [0x0061 ..]

xkBraceleftToAsciitilde :: DecsQ
xkBraceleftToAsciitilde = concatR <$> zipWithM (flip mkXk) [0x007b ..] [
	"braceleft", "bar", "braceright", "asciitilde" ]

xkModifiers :: DecsQ
xkModifiers = concatR <$> zipWithM (flip mkXk) [0xffe1 .. 0xffee] [
	"Shift_L", "Shift_R", "Control_L", "Control_R", "Caps_Lock", "Shift_Lock",
	"Meta_L", "Meta_R", "Alt_L", "Alt_R", "Super_L", "Super_R", "Hyper_L", "Hyper_R"
	]

xkIsoLeftTab :: DecsQ
xkIsoLeftTab = mkXk "ISO_Left_Tab" 0xfe20

mkXk :: String -> Integer -> DecsQ
mkXk s n = do
	let	xkn = mkName $ "XK_" ++ s
	sequence [
		patSynSigD xkn $ conT ''Key,
		patSynD xkn (prefixPatSyn []) implBidir (conP 'Key [litP $ integerL n])
		]
