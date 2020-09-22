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

import Graphics.Gtk

import Data.Type.Flip
import Data.OneOfThem as Oot
import Data.Or

import qualified Data.Set as D
import qualified Data.List as L

import Trial.Draw.OneOfThem
import Trial.Draw.Event
import Trial.Draw.Handle
import Trial.Draw.Viewable

import Debug.Trace

type Viewable = OneOfThem (Box :- Line :- Message :- 'Nil)
type Events = GetThreadId :- MouseEv :+: LockEv :+: LinesEv

first' :: Firstable es es' a a => React s es a -> React s es' a -> React s (es :+: es') a
first' l r = first l r >>= \case
	L x -> pure x
	R y -> pure y
	LR x _ -> pure x

maybeEither :: b -> Either a (Maybe b, ()) -> b
maybeEither d (Left _) = d
maybeEither d (Right (Nothing, ())) = d
maybeEither _ (Right (Just x, ())) = x

maybeEither2 :: r' -> Either r (Maybe a, r') -> r'
maybeEither2 d (Left _) = d
maybeEither2 _ (Right (_, x)) = x

rectangleAndLines :: Sig s Events [Viewable] ()
rectangleAndLines = do
	li0 <- waitFor $ adjust newLockId
	li <- waitFor $ adjust newLockId
	(sortType @('[Box, Line, Message]) <$%>) $ (\yr ls bx -> yr : ls ++ bx)
		<$%> (emit (Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Yellow) >> waitFor never)
		<*%> (emit [] >> sampleLine li0 li)
		<*%> do	emit []
			waitFor (adjust clickOnBox)
			emit [	Oot.expand . Singleton $ Message "Yellow Box have clicked",
				Oot.expand . Singleton $ Box (Rect (200, 200) (250, 250)) Red]
			waitFor never

clickOnBox :: React s MouseEv ()
clickOnBox = void . adjust $ find (`insideRect` Rect (50, 50) (100, 100)) (mousePos `indexBy` repeat leftClick)

clickOnRect, upOnRect, rightOnRect :: Rect -> React s MouseEv ()
clickOnRect r = void . adjust $ find (`insideRect` r) (mousePos `indexBy` repeat leftClick)
upOnRect r = void . adjust $ find (`insideRect` r) (mousePos `indexBy` repeat leftUp)
rightOnRect r = void . adjust $ find (`insideRect` r) (mousePos `indexBy` repeat rightClick)

insideRect :: Point -> Rect -> Bool
insideRect (x, y) (Rect (l, t) (r, b)) = l <= x && x <= r && t <= y && y <= b

sampleLine :: LockId -> LockId -> Sig s Events [Viewable] ()
sampleLine li0 li = do
	_ <- (concat <$%>) .  parList $ spawn do
		(s, e) <- withLockSig li0 do
			s <- waitFor clickPoint
			e <- maybeEither2 (0, 0) <$> (makeLineAndPoint s <$%> mousePos `break` upPoint)
			waitFor . adjust $ addLine li (s, e)
			pure (s, e)
		(\l ss es -> l ++ ss ++ es)
			<$%> (emit (makeLine s e) >> waitFor never)
			<*%> do	emit $ makePoint Yellow s
				void . waitFor $ pointToReactRight s
				emit $ makePoint Red s
				waitFor never'
			<*%> do	emit $ makePoint Yellow e
				void . waitFor $ pointToReactRight e
				emit $ makePoint Red e
				waitFor never'
	waitFor never

never' :: React s es ()
never' = never

clickPoint :: React s Events Position
clickPoint = do
	ls <- adjust loadLines
	let	r = trace ("clickPoint: " ++ show (linesToPoints ls) ++ "\n") $ linesToReact ls
	r `first'` (maybeEither (0, 0) <$> mousePos `at` leftClick)

upPoint :: React s Events Position
upPoint = do
	ls <- adjust loadLines
	let	r = trace ("upPoint: " ++ show (linesToPoints ls) ++ "\n") $ linesToReactUp ls
	r `first'` (maybeEither (0, 0) <$> mousePos `at` leftUp)

makeLineAndPoint, makeLine :: Point -> Point -> [Viewable]
makeLineAndPoint s e = makePoint Yellow s ++ makePoint Yellow e ++ makeLine s e
makeLine s e = [Oot.expand . Singleton $ Line' (Color 0 0 0) 2 s e]

makePoint :: BColor -> Point -> [Viewable]
makePoint c (x, y) = [
	Oot.expand . Singleton $ Box (Rect (x - 5, y - 5) (x + 5, y + 5)) c ]

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

runDraw :: (Monoid a, Adjustable es (Events :+: GuiEv)) => GtkDrawer a -> Sig s es a r -> IO (r, DrawState)
runDraw dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpretSt (retrySt $
		(liftHandle' handleGetThreadId `mergeSt`
		handleLock `mergeSt`
		handleLines) `beforeSt`
		liftHandle' (handle Nothing cr c)
		) c' s (DrawState 0 [] D.empty) <* gtkMainQuit

linesToPoints :: D.Set SimpleLine -> [Position]
linesToPoints = concatMap (\(x, y) -> [x, y]) . D.toList

pointToReact, pointToReactUp, pointToReactRight :: Position -> React s Events Position
pointToReact (x, y) = do
	adjust . clickOnRect $ Rect (x - 10, y - 10) (x + 10, y + 10)
	pure (x, y)

pointToReactUp (x, y) = do
	adjust . upOnRect $ Rect (x - 10, y - 10) (x + 10, y + 10)
	pure (x, y)

pointToReactRight (x, y) = do
	adjust . rightOnRect $ Rect (x - 10, y - 10) (x + 10, y + 10)
	pure (x, y)

linesToReact, linesToReactUp :: D.Set SimpleLine -> React s Events Position
linesToReact = foldr first' never . map pointToReact . linesToPoints
linesToReactUp = foldr first' never . map pointToReactUp . linesToPoints
