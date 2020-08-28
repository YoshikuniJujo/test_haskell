{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.Clickable (
	-- * Clickable
	Clickable, view, click, clickable, clickableText,
	-- * With Text Extents
	WithTextExtents, withTextExtents, nextToText, translate
	) where

import Prelude hiding (repeat)

import Control.Moffy (React, adjust, repeat, find, indexBy)
import Control.Moffy.Event.Mouse (MouseEv, leftClick, mouseMove)
import Data.Type.Set (Singleton)
import Graphics.X11.Xrender (XGlyphInfo(..))

import qualified Data.Text as T

import Trial.Followbox.Event (CalcTextExtents, calcTextExtents)
import Trial.Followbox.ViewType (View, View1(..), blue)
import Trial.Followbox.TypeSynonym (Position, FontName, FontSize)

---------------------------------------------------------------------------

-- * CLICKABLE
-- * WITH TEXT EXTENTS

---------------------------------------------------------------------------
-- CLICKABLE
---------------------------------------------------------------------------

data Clickable s = Clickable { view :: View, click :: React s MouseEv () }

clickable :: View -> Position -> Position -> Clickable s
clickable v (l, t) (r, b) = Clickable v
	. adjust $ () <$ find isd (repeat mouseMove `indexBy` repeat leftClick)
	where isd (x, y) = l <= x && x <= r && t <= y && y <= b

---------------------------------------------------------------------------
-- WITH TEXT EXTENTS
---------------------------------------------------------------------------

data WithTextExtents = WithTextExtents FontName FontSize T.Text XGlyphInfo

clickableText :: Position -> WithTextExtents -> Clickable s
clickableText p@(x, y) (WithTextExtents fn fs txt xg) =
	clickable [Text blue fn fs p txt] (l, t) (l + gw, t + gh) where
	(l, t) = (x - gx, y - gy)
	[gx, gy, gw, gh] = fromIntegral . ($ xg) <$> [
		xglyphinfo_x, xglyphinfo_y,
		xglyphinfo_width, xglyphinfo_height ]

withTextExtents :: FontName -> FontSize -> T.Text ->
	React s (Singleton CalcTextExtents) WithTextExtents
withTextExtents fn fs t = WithTextExtents fn fs t <$> calcTextExtents fn fs t

nextToText :: Position -> WithTextExtents -> Position
nextToText (x, y) (WithTextExtents _ _ _ xg) = (x + xo, y + yo) where
	[xo, yo] = fromIntegral . ($ xg) <$> [xglyphinfo_xOff, xglyphinfo_yOff]

translate :: Position -> WithTextExtents -> (Rational, Rational) -> Position
translate (x, y) (WithTextExtents _ (toRational -> fs) _ _) (dx, dy) =
	(x + round (fs * dx), y + round (fs * dy))
