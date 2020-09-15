{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.Clickable (
	-- * Clickable
	Clickable, view, click, clickable, clickableText,
	-- * With Text Extents
	WithTextExtents, withTextExtents, nextToText, translate,
	-- * Temporary
	FontName, FontSize
	) where

import Prelude hiding (repeat)

import Control.Moffy (React, adjust, repeat, find, indexBy)
import Control.Moffy.Event.Mouse (MouseEv, leftClick, mouseMove)
import Control.Moffy.Event.CalcTextExtents (
	TextExtents'(..), FontName, FontSize, Rectangle(..),
	CalcTextExtents, calcTextExtents' )
import Data.Type.Set (Singleton)

import qualified Data.Text as T

import Trial.Followbox.ViewType (View(..), View1(..), blue, VText(..), Line(..), Image(..))
import Trial.Followbox.Basic (Position)

import Data.OneOfThem

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

data WithTextExtents = WithTextExtents FontName FontSize T.Text TextExtents'

clickableText :: Position -> WithTextExtents -> Clickable s
clickableText p@(x, y) (WithTextExtents fn fs txt xg) =
	clickable (View [expand . Singleton $ Text' blue fn fs p txt]) (l, t) (l + gw, t + gh) where
	(l, t) = (x, y)
	[gw, gh] = ($ xg) <$> [
		rectangleWidth . textExtentsInkRect, rectangleHeight . textExtentsInkRect ]

withTextExtents :: FontName -> FontSize -> T.Text ->
	React s (Singleton CalcTextExtents) WithTextExtents
withTextExtents fn fs t = WithTextExtents fn fs t <$> calcTextExtents' fn fs t

nextToText :: Position -> WithTextExtents -> Position
nextToText (x, y) (WithTextExtents _ _ _ xg) = (x + xo, y) where
	[xo, _yo] = ($ xg) <$> [rectangleWidth . textExtentsLogicalRect, rectangleHeight . textExtentsLogicalRect]

translate :: Position -> WithTextExtents -> (Rational, Rational) -> Position
translate (x, y) (WithTextExtents _ (toRational -> fs) _ _) (dx, dy) =
	(x + fromRational (fs * dx), y + fromRational (fs * dy))
