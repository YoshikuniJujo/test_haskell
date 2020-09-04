{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.TextExtents where

import qualified Graphics.X11.Xrender as X

data TextExtents = TextExtents {
	textExtentsXBearing :: Double,
	textExtentsYBearing :: Double,
	textExtentsWidth :: Double,
	textExtentsHeight :: Double,
	textExtentsXAdvance :: Double,
	textExtentsYAdvance :: Double } deriving Show

xGlyphInfoToX :: TextExtents -> X.XGlyphInfo
xGlyphInfoToX (TextExtents x_ y_ w_ h_ xo_ yo_) = X.XGlyphInfo w h x y xo yo
	where [w, h, x, y, xo, yo] = round <$> [w_, h_, x_, y_, xo_, yo_]

xGlyphInfoToNew :: X.XGlyphInfo -> TextExtents
xGlyphInfoToNew (X.XGlyphInfo w_ h_ x_ y_ xo_ yo_) = TextExtents x y w h xo yo
	where [w, h, x, y, xo, yo] = fromIntegral <$> [w_, h_, x_, y_, xo_, yo_]
