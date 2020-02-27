{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module XGlyphInfo (XGlyphInfo(..), convertXGlyphInfo) where

import qualified Graphics.X11.Xrender as Xr

data XGlyphInfo = XGlyphInfo {
	xGlyphInfoWidth :: Int,
	xGlyphInfoHeight :: Int,
	xGlyphInfoX :: Int,
	xGlyphInfoY :: Int,
	xGlyphInfoXOff :: Int,
	xGlyphInfoYOff :: Int
	} deriving (Show, Eq, Ord)

convertXGlyphInfo :: Xr.XGlyphInfo -> XGlyphInfo
convertXGlyphInfo (Xr.XGlyphInfo w h x y xo yo) = XGlyphInfo w h x y xo yo
