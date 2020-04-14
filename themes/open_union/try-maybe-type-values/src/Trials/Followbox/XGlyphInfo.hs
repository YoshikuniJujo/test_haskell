{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.XGlyphInfo (XGlyphInfo(..), textExtents) where

import qualified Data.Text as T
import qualified Graphics.X11.Xrender as Xr

import qualified Field as F

data XGlyphInfo = XGlyphInfo {
	xGlyphInfoWidth :: Integer, xGlyphInfoHeight :: Integer,
	xGlyphInfoX :: Integer, xGlyphInfoY :: Integer,
	xGlyphInfoXOff :: Integer, xGlyphInfoYOff :: Integer
	} deriving (Show, Eq, Ord)

convertXGlyphInfo :: Xr.XGlyphInfo -> XGlyphInfo
convertXGlyphInfo (Xr.XGlyphInfo w_ h_ x_ y_ xo_ yo_) = XGlyphInfo w h x y xo yo
	where [w, h, x, y, xo, yo] = fromIntegral <$> [w_, h_, x_, y_, xo_, yo_]

textExtents :: F.Field -> String -> Double -> T.Text -> IO XGlyphInfo
textExtents f fn fs t = convertXGlyphInfo <$> F.textExtents f fn fs (T.unpack t)
