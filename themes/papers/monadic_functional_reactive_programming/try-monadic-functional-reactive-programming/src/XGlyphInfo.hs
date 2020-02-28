{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module XGlyphInfo (XGlyphInfo(..), convertXGlyphInfo) where

import qualified Graphics.X11.Xrender as Xr

data XGlyphInfo n = XGlyphInfo {
	xGlyphInfoWidth :: n,
	xGlyphInfoHeight :: n,
	xGlyphInfoX :: n,
	xGlyphInfoY :: n,
	xGlyphInfoXOff :: n,
	xGlyphInfoYOff :: n
	} deriving (Show, Eq, Ord)

convertXGlyphInfo :: Num n => Xr.XGlyphInfo -> XGlyphInfo n
convertXGlyphInfo (Xr.XGlyphInfo w_ h_ x_ y_ xo_ yo_) = XGlyphInfo w h x y xo yo
	where [w, h, x, y, xo, yo] = fromIntegral <$> [w_, h_, x_, y_, xo_, yo_]
