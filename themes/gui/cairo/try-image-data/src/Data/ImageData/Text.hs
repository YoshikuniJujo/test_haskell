{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData.Text where

data Font
	= Font {
		fontFamily :: String,
		fontStyle :: FontStyle,
		fontVariant :: FontVariant,
		fontWeight :: FontWeight,
		fontStretch :: FontStretch }
	| VariableFont {
		variableFontFamily :: String
		}
	deriving Show

data FontStyle = StyleNormal | StyleOblique | StyleItalic deriving Show

data FontVariant = VariantNormal | VariantSmallCaps deriving Show

data FontWeight
	= WeightThin | WeightUltralight | WeightLight | WeightSemilight
	| WeightBook | WeightNormal | WeightMedium | WeightSemibold
	| WeightBold | WeightUltrabold | WeightHeavy | WeightUltraheavy
	deriving (Show, Enum)

data FontStretch
	= StretchUltraCondensed | StretchExtraCondensed | StretchCondensed
	| StretchSemiCondensed | StretchNormal | StretchSemiExpanded
	| StretchExpanded | StretchExtraExpanded | StretchUltraExpanded
	deriving Show
