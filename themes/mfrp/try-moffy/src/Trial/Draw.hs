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
import Control.Moffy.Event.ThreadId
import Control.Moffy.Event.Lock
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Mouse
import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Shape
import Data.Type.Set

import Control.Moffy.Handle
import Control.Moffy.Handle.ThreadId
import Control.Moffy.Handle.Lock
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField

import Graphics.Gtk hiding (DeleteEvent)

import Data.Type.Flip
import Data.OneOfThem as Oot
import Data.Or

import qualified Data.Set as D
import qualified Data.List as L

import Trial.Draw.OneOfThem
import Trial.Draw.Event
import Trial.Draw.Handle
import Trial.Draw.Viewable

type Viewable = OneOfThem (Box :- Line :- Message :- 'Nil)
type Events = GetThreadId :- DeleteEvent :- MouseEv :+: LockEv :+: LinesEv

first' :: Firstable es es' a a => React s es a -> React s es' a -> React s (es :+: es') a
first' l r = first l r >>= \case
	L x -> pure x
	R y -> pure y
	LR x _ -> pure x

maybeEither :: b -> Either a (Maybe b, ()) -> b
maybeEither d (Left _) = d
maybeEither d (Right (Nothing, ())) = d
maybeEither _ (Right (Just x, ())) = x

rectangleAndLines :: Sig s Events [Viewable] ()
rectangleAndLines = (sortType @('[Box, Line, Message]) <$%>) $ (\yr ls bx -> yr : ls ++ bx)
	<$%> (emit (Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Yellow) >> waitFor (adjust deleteEvent))
	<*%> (emit [] >> sampleLine)
	<*%> do
		emit []
		waitFor (adjust clickOnBox)
		emit [	Oot.expand . Singleton $ Message "Yellow Box have clicked",
			Oot.expand . Singleton $ Box (Rect (200, 200) (250, 250)) Red]
		waitFor (adjust deleteEvent)

clickOnBox :: React s MouseEv ()
clickOnBox = void . adjust $ find (`insideRect` Rect (50, 50) (100, 100)) (mousePos `indexBy` repeat leftClick)

clickOnRect :: Rect -> React s MouseEv ()
clickOnRect r = void . adjust $ find (`insideRect` r) (mousePos `indexBy` repeat leftClick)

insideRect :: Point -> Rect -> Bool
insideRect (x, y) (Rect (l, t) (r, b)) = l <= x && x <= r && t <= y && y <= b

sampleLine :: Sig s Events [Viewable] ()
sampleLine = do
	_ <- (concat <$%>) .  parList $ spawn do
		s <- waitFor . adjust $ maybeEither (0, 0) <$> mousePos `at` leftClick
		e <- maybeEither undefined <$> (makeLine s <$%> adjustSig (mousePos `break` leftUp))
		emit $ makeLine s e

		waitFor do
			li <- adjust newLockId
			adjust $ addLine li (s, e)
		waitFor $ adjust deleteEvent
	waitFor $ adjust deleteEvent

makeLine :: Point -> Point -> [Viewable]
makeLine s@(xs, ys) e@(xe, ye) = [
	Oot.expand . Singleton $ Box (Rect (xs - 5, ys - 5) (xs + 5, ys + 5)) Yellow,
	Oot.expand . Singleton $ Box (Rect (xe - 5, ye - 5) (xe + 5, ye + 5)) Yellow,
	Oot.expand . Singleton $ Line' (Color 0 0 0) 2 s e ]

data DrawState = DrawState {
	dsNextLockId :: Int,
	dsLocks :: [LockId],
	dsLines :: D.Set SimpleLine }
	deriving Show

instance LockState DrawState where
	getNextLockId = dsNextLockId
	putNextLockId s li = s { dsNextLockId = li }
	isLocked s li = li `elem` dsLocks s
	lockIt s li = s { dsLocks = li : dsLocks s }
	unlockIt s li = s { dsLocks = L.delete li (dsLocks s) }

instance LinesState DrawState where
	getLines = dsLines
	putLines s ls = s { dsLines = ls }

runDraw :: Monoid a => GtkDrawer a -> Sig s Events a r -> IO (r, DrawState)
runDraw dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpretSt (retrySt $
		liftHandle' handleGetThreadId `mergeSt`
		handleLock `mergeSt`
		handleLines `beforeSt`
		liftHandle' (handle Nothing cr c)
		) c' s (DrawState 0 [] D.empty) <* gtkMainQuit
