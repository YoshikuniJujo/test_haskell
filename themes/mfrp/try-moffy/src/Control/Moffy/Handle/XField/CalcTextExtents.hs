{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField.CalcTextExtents where

import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle
import Data.Type.Set
import Data.OneOrMore as Oom

import qualified Data.Text as T
import qualified Graphics.X11.Xrender as X

import Field

import Data.OneOrMoreApp as Ooma (pattern Singleton)

handleCalcTextExtents :: Field -> Handle IO (Singleton CalcTextExtents)
handleCalcTextExtents f (Oom.Singleton (CalcTextExtentsReq wid fn fs t)) = Ooma.Singleton
	. OccCalcTextExtents wid fn fs t . mkTextExtents . xGlyphInfoToNew <$> textExtents f fn fs (T.unpack t)

xGlyphInfoToNew :: X.XGlyphInfo -> TextExtents'
xGlyphInfoToNew (X.XGlyphInfo w_ h_ x_ y_ xo_ yo_) = TextExtents' x y w h xo yo
	where [w, h, x, y, xo, yo] = fromIntegral <$> [w_, h_, x_, y_, xo_, yo_]

data TextExtents' = TextExtents' {
	textExtentsXBearing :: Double,
	textExtentsYBearing :: Double,
	textExtentsWidth :: Double,
	textExtentsHeight :: Double,
	textExtentsXAdvance :: Double,
	textExtentsYAdvance :: Double } deriving Show

mkTextExtents :: TextExtents' -> TextExtents
mkTextExtents e = TextExtents {
	textExtentsInkRect = Rectangle l t w h,
	textExtentsLogicalRect = Rectangle l t w' h' }
	where
	l = 0
	t = 0
	w = textExtentsWidth e
	h = textExtentsHeight e
	w' = textExtentsXAdvance e + textExtentsXBearing e
	h' = textExtentsYAdvance e + textExtentsYBearing e
