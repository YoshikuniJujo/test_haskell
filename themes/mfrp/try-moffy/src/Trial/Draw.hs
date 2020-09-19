{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw where

import Prelude hiding (repeat, break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Mouse
import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Shape
import Data.Type.Set

import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField

import Graphics.Gtk hiding (DeleteEvent)

import Data.Type.Flip
import Data.OneOfThem as Oot
import Data.Or

import Trial.Draw.OneOfThem

first' :: Firstable es es' a a => React s es a -> React s es' a -> React s (es :+: es') a
first' l r = first l r >>= \case
	L x -> pure x
	R y -> pure y
	LR x _ -> pure x

maybeEither :: b -> Either a (Maybe b, ()) -> b
maybeEither d (Left _) = d
maybeEither d (Right (Nothing, ())) = d
maybeEither _ (Right (Just x, ())) = x

rectangleAndLines :: Sig s (DeleteEvent :- MouseEv) [OneOfThem (Box :- Line :- 'Nil)] ()
rectangleAndLines = (sortType @('[Box, Line]) <$%>) $ (\yr ls bx -> yr : ls ++ bx)
	<$%> (emit (Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Yellow) >> waitFor (adjust deleteEvent))
	<*%> sampleLine
	<*%> do
		emit []
		waitFor (adjust clickOnBox)
		emit [Oot.expand . Singleton $ Box (Rect (200, 200) (250, 250)) Red]
		waitFor (adjust deleteEvent)

clickOnBox :: React s MouseEv ()
clickOnBox = void . adjust $ find (`insideRect` Rect (50, 50) (100, 100)) (mousePos `indexBy` repeat leftClick)

clickOnRect :: Rect -> React s MouseEv ()
clickOnRect r = void . adjust $ find (`insideRect` r) (mousePos `indexBy` repeat leftClick)

insideRect :: Point -> Rect -> Bool
insideRect (x, y) (Rect (l, t) (r, b)) = l <= x && x <= r && t <= y && y <= b

sampleLine :: Sig s (DeleteEvent :- MouseEv) [OneOfThem (Box :- Line :- 'Nil)] ()
sampleLine = do
	_ <- (concat <$%>) .  parList $ spawn do
		s <- waitFor . adjust $ maybeEither (0, 0) <$> mousePos `at` leftClick
		_ <- makeLine s <$%> adjustSig (mousePos `break` leftUp)
		waitFor $ adjust deleteEvent
	waitFor $ adjust deleteEvent

makeLine :: Point -> Point -> [OneOfThem (Box :- Line :- 'Nil)]
makeLine s@(xs, ys) e@(xe, ye) = [
	Oot.expand . Singleton $ Box (Rect (xs - 5, ys - 5) (xs + 5, ys + 5)) Yellow,
	Oot.expand . Singleton $ Box (Rect (xe - 5, ye - 5) (xe + 5, ye + 5)) Yellow,
	Oot.expand . Singleton $ Line' (Color 0 0 0) 2 s e ]

runDraw :: Monoid a => GtkDrawer a -> Sig s (DeleteEvent :- MouseEv) a r -> IO r
runDraw dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpret (retry $ handle Nothing cr c) c' s <* gtkMainQuit
