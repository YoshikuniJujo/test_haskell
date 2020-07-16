{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.Internal where

import Language.Haskell.TH
import Control.Monad
import Data.Word

newtype Key = Key Word32 deriving (Show, Eq)

xkSpaceToSlash :: DecsQ
xkSpaceToSlash = concat <$> zipWithM (flip mkXk) [0x0020 ..] [
	"space", "exclam", "quotedbl", "numbersign", "dollar", "percent",
	"ampersand", "apostrophe", "parenleft", "parenright", "asterisk",
	"plus", "comma", "minus", "period", "slash" ]

xkDigit :: DecsQ
xkDigit = concat <$> zipWithM mkXk ((: []) <$> ['0' .. '9']) [0x0030 ..]

xkColonToAt :: DecsQ
xkColonToAt = concat <$> zipWithM (flip mkXk) [0x003a ..] [
	"colon", "semicolon", "less", "equal", "greater", "question", "at" ]

xkUpperAlph :: DecsQ
xkUpperAlph = concat <$> zipWithM mkXk ((: []) <$> ['A' .. 'Z']) [0x0041 ..]

xkBlacketleftToGrave :: DecsQ
xkBlacketleftToGrave = concat <$> zipWithM (flip mkXk) [0x005b ..] [
	"bracketleft", "backslash", "bracketright", "asciicircum", "underscore",
	"grave" ]

xkLowerAlph :: DecsQ
xkLowerAlph = concat <$> zipWithM mkXk ((: []) <$> ['a' .. 'z']) [0x0061 ..]

xkBraceleftToAsciitilde :: DecsQ
xkBraceleftToAsciitilde = concat <$> zipWithM (flip mkXk) [0x007b ..] [
	"braceleft", "bar", "braceright", "asciitilde" ]

mkXk :: String -> Integer -> DecsQ
mkXk s n = do
	let	xkn = mkName $ "XK_" ++ s
	sequence [
		patSynSigD xkn $ conT ''Key,
		patSynD xkn (prefixPatSyn []) implBidir (conP 'Key [litP $ integerL n])
		]
