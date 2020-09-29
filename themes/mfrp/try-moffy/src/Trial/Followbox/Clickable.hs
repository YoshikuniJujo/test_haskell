{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
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
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow (MouseEv, leftClick, mouseMove)
import Control.Moffy.Event.CalcTextExtents.DefaultWindow (
	TextExtents(..), FontName, FontSize, Rectangle(..),
	CalcTextExtents, calcTextExtents )
import Data.Type.Set (pattern Nil, (:-))

import qualified Data.Text as T

import Trial.Followbox.ViewType (View(..), blue, VText(..))
import Control.Moffy.Viewable.Basic (Position)

import Data.OneOfThem

import Control.Moffy.Event.Window

---------------------------------------------------------------------------

-- * CLICKABLE
-- * WITH TEXT EXTENTS

---------------------------------------------------------------------------
-- CLICKABLE
---------------------------------------------------------------------------

data Clickable s = Clickable { view :: View, click :: React s (LoadDefaultWindow :- MouseEv) () }

clickable :: View -> Position -> Position -> Clickable s
clickable v (l, t) (r, b) = Clickable v
	. adjust $ () <$ find isd (repeat mouseMove `indexBy` repeat leftClick)
	where isd (x, y) = l <= x && x <= r && t <= y && y <= b

---------------------------------------------------------------------------
-- WITH TEXT EXTENTS
---------------------------------------------------------------------------

data WithTextExtents = WithTextExtents FontName FontSize T.Text TextExtents

clickableText :: Position -> WithTextExtents -> Clickable s
clickableText p@(x, y) (WithTextExtents fn fs txt xg) =
	clickable (View [expand . Singleton $ Text' blue fn fs p txt]) (l, t) (l + gw, t + gh) where
	(l, t) = (x + dx, y + dy)
	[dx, dy, gw, gh] = ($ textExtentsInkRect xg) <$> [
		rectangleLeft, rectangleTop, rectangleWidth, rectangleHeight ]

withTextExtents :: FontName -> FontSize -> T.Text ->
	React s (LoadDefaultWindow :- CalcTextExtents :- 'Nil) WithTextExtents
withTextExtents fn fs t = WithTextExtents fn fs t <$> calcTextExtents fn fs t

nextToText :: Position -> WithTextExtents -> Position
nextToText (x, y) (WithTextExtents _ _ _ xg) = (x + xo, y) where
	[xo, _yo] = ($ xg) <$> [rectangleWidth . textExtentsLogicalRect, rectangleHeight . textExtentsLogicalRect]

translate :: Position -> WithTextExtents -> (Rational, Rational) -> Position
translate (x, y) (WithTextExtents _ (toRational -> fs) _ _) (dx, dy) =
	(x + fromRational (fs * dx), y + fromRational (fs * dy))
