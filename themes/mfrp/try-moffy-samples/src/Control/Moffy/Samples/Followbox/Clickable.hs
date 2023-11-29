{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox.Clickable (
	-- * Clickable
	Clickable, view, click, clickable, clickableText,
	-- * With Text Extents
	WithTextExtents, withTextExtents, nextToText, translate,
	-- * Temporary
	FontName, FontSize
	) where

import Prelude hiding (repeat)

import Control.Moffy (React, adjust, repeat, find, indexBy)
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Followbox.Event.CalcTextExtents (
	TextExtents(..), FontName, FontSize, Rectangle(..),
	CalcTextExtents, calcTextExtents )
import Data.Type.Set (pattern Nil, (:-), Singleton)

import qualified Data.Text as T

import Trial.Followbox.ViewType (View(..), blue, VText(..))
import Control.Moffy.Viewable.Basic (Position)

import Data.OneOfThem

import Data.Type.Flip ((<$%>))

import Data.Bool

---------------------------------------------------------------------------

-- * CLICKABLE
-- * WITH TEXT EXTENTS

---------------------------------------------------------------------------
-- CLICKABLE
---------------------------------------------------------------------------

-- data Clickable s = Clickable { view :: View, click :: React s (LoadDefaultWindow :- MouseEv) () }
data Clickable s = Clickable { view :: View, click :: React s MouseEv () }

type MouseEv = Mouse.Move :- Mouse.Down :- Mouse.Up :- 'Nil

clickable :: View -> Position -> Position -> Clickable s
clickable v (l, t) (r, b) = Clickable v
	. adjust $ () <$ find isd (fst <$%> repeat Mouse.move `indexBy` repeat leftClick)
	where isd (x, y) = l <= x && x <= r && t <= y && y <= b

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b0 = do
	b <- Mouse.down
	bool (clickOn b0) (pure ()) (b == b0)

leftClick :: React s (Singleton Mouse.Down) ()
leftClick = clickOn Mouse.ButtonPrimary

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
--	React s (LoadDefaultWindow :- CalcTextExtents :- 'Nil) WithTextExtents
	React s (CalcTextExtents :- 'Nil) WithTextExtents
withTextExtents fn fs t = WithTextExtents fn fs t <$> calcTextExtents fn fs t

nextToText :: Position -> WithTextExtents -> Position
nextToText (x, y) (WithTextExtents _ _ _ xg) = (x + xo, y) where
	[xo, _yo] = ($ xg) <$> [rectangleWidth . textExtentsLogicalRect, rectangleHeight . textExtentsLogicalRect]

translate :: Position -> WithTextExtents -> (Rational, Rational) -> Position
translate (x, y) (WithTextExtents _ (toRational -> fs) _ _) (dx, dy) =
	(x + fromRational (fs * dx), y + fromRational (fs * dy))
