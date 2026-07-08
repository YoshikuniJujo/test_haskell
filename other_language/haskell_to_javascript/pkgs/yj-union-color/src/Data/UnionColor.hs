{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.UnionColor (

	-- * ALPHA

	Alpha, pattern AlphaWord8, pattern AlphaWord16, pattern AlphaWord32,
	pattern AlphaInt32, alphaInt32,
	pattern AlphaDouble, alphaDouble, alphaRealToFrac,

	-- * RGB

	Rgb, pattern RgbWord8, pattern RgbWord16, pattern RgbWord32,
	pattern RgbInt32, rgbInt32,
	pattern RgbDouble, rgbDouble, rgbRealToFrac,

	-- * RGBA

	-- ** Straight

	Rgba, pattern RgbaWord8, pattern RgbaWord16, pattern RgbaWord32,
	pattern RgbaInt32, rgbaInt32,
	pattern RgbaDouble, rgbaDouble,

	-- ** Premultiplied

	pattern RgbaPremultipliedWord8, rgbaPremultipliedWord8,
	pattern RgbaPremultipliedWord16, rgbaPremultipliedWord16,
	pattern RgbaPremultipliedDouble, rgbaPremultipliedDouble,

	-- ** Raw

	RgbaRaw,
	pattern RgbaWord8Raw, pattern RgbaWord16Raw,
	pattern RgbaWord32Raw, pattern RgbaInt32Raw,
	pattern RgbaDoubleRaw, rgbaDoubleRaw,

	rawAsStraight, rawAsPremultiplied,
	straightToRaw, premultipliedToRaw,

	-- ** From and To Rgb and Alpha

	toRgba, fromRgba,

	-- ** Convert Fractional

	rgbaRealToFrac,

	-- * GRAY SCALE

	Gray,
	pattern GrayWord1, grayWord1, pattern GrayWord2, grayWord2,
	pattern GrayWord4, grayWord4,
	pattern GrayWord8, pattern GrayWord16, pattern GrayWord32,
	pattern GrayInt32,
	pattern GrayDouble, grayDouble,

	-- ** Convert Fractional

	grayRealToFrac,

	-- * GRAY SCALE WITH ALPHA

	GrayAlpha,
	pattern GrayAlphaWord8, pattern GrayAlphaWord16,
	pattern GrayAlphaWord32, pattern GrayAlphaInt32,
	pattern GrayAlphaDouble, grayAlphaDouble,

	-- ** From and To Gray and Alpha

	toGrayAlpha, fromGrayAlpha,

	-- ** Convert Fractional

	grayAlphaRealToFrac

	) where

import Data.UnionColor.Internal
import Data.UnionColor.Internal.GrayScale
