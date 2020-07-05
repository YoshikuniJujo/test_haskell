{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.Clickable (
	-- * CLICKABLE
	Clickable, view, click, clickable, clickableText,
	-- * WITH TEXT EXTENTS
	WithTextExtents, withTextExtents, nextToText, translate
	) where

import Prelude hiding (repeat)

import Control.Moffy
import Graphics.X11.Xrender (XGlyphInfo(..))

import qualified Data.Text as T

import Control.Moffy.Event.Mouse (leftClick, mouseMove)
import Trial.Followbox.Event (SigF, ReactF, calcTextExtents)
import Trial.Followbox.View (View, View1(..), blue)
import Trial.Followbox.TypeSynonym (Position, FontName, FontSize)

---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- CLICKABLE
---------------------------------------------------------------------------

data Clickable s = Clickable View (ReactF s ())

view :: Clickable s -> View
view (Clickable v _) = v

click :: Clickable s -> ReactF s ()
click (Clickable _ r) = r

clickable :: View -> Position -> Position -> Clickable s
clickable v ul br = Clickable v $ clickOn ul br

clickOn :: Position -> Position -> ReactF s ()
clickOn (l, t) (r, b) =
	() <$ find inside (mousePosition `indexBy` repeat leftClick)
	where inside (x, y) = l <= x && x <= r && t <= y && y <= b

mousePosition :: SigF s Position ()
mousePosition = repeat $ adjust mouseMove

---------------------------------------------------------------------------
-- WITH TEXT EXTENTS
---------------------------------------------------------------------------

data WithTextExtents = WithTextExtents FontName FontSize T.Text XGlyphInfo

clickableText :: Position -> WithTextExtents -> Clickable s
clickableText p@(x, y) (WithTextExtents fn fs t xg) =
	clickable [Text blue fn fs p t] (left, top) (left + gw, top + gh)
	where
	(left, top) = (x - gx, y - gy)
	[gx, gy, gw, gh] = fromIntegral . ($ xg) <$> [
		xglyphinfo_x, xglyphinfo_y,
		xglyphinfo_width, xglyphinfo_height ]

withTextExtents :: FontName -> FontSize -> T.Text -> ReactF s WithTextExtents
withTextExtents fn fs t =
	WithTextExtents fn fs t <$> adjust (calcTextExtents fn fs t)

translate :: Position -> WithTextExtents -> (Rational, Rational) -> Position
translate (x, y) (WithTextExtents _ fs _ _) (dx, dy) =
	(x + round (fs' * dx), y + round (fs' * dy)) where fs' = toRational fs

nextToText :: Position -> WithTextExtents -> Position
nextToText (x, y) (WithTextExtents _ _ _ xg) = (x + xo, y + yo) where
	[xo, yo] = fromIntegral . ($ xg) <$> [xglyphinfo_xOff, xglyphinfo_yOff]
