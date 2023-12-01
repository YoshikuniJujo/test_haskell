{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (until, repeat, cycle)

import Control.Arrow qualified as A
import Control.Monad
import Control.Monad.Trans.Except (pattern ExceptT, runExceptT)
import Control.Concurrent
import Control.Concurrent.STM (atomically, newTChan, readTChan, writeTChan)
import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Handle qualified as H
import Control.Moffy.Samples.Handle.TChan
import Control.Moffy.Handle.Time
import Control.Moffy.Samples.Run.TChan
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.View qualified as V
import Control.Moffy.Samples.Run.Gtk4
import Control.Moffy.Samples.Followbox.Event.CalcTextExtents
import Data.Type.Set
import Data.Type.Flip
import Data.OneOrMoreApp
import Data.Or
import Data.Bool
import Data.List.NonEmpty (fromList)
import Data.List.Infinite (Infinite(..), cycle)
import Data.Maybe
import Data.Color
import Data.Time
import Data.Time.Clock.System
import Data.Time.Clock.TAI

main :: IO ()
main = do
	er <- atomically newTChan
	eo <- atomically newTChan
	v <- atomically newTChan
	void $ forkIO do
		now <- systemToTAITime <$> getSystemTime
		($ (InitialMode, now)) $ interpretSt
			(H.retrySt . ($ (0.05, ())) . H.popInput . handleTimeEvPlus . H.pushInput . const . H.liftHandle' . H.sleepIfNothing 50000
				$ handleNew @(CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent) er eo)
			v do
			boxesToView <$%> boxes `until` deleteEvent
--			rectToView <$%> defineRect
--			rectToView <$%> curRect (150, 100) `until` deleteEvent
--			rectToView <$%> wiggleRect (Rect (150, 100) (300, 200)) `until` deleteEvent
--			waitFor $ doubler `first` deleteEvent
			emit V.Stopped
		putStrLn "AFTER INTERPRET"
	runSingleWin er eo v

sameClick :: React s (Singleton Mouse.Down) Bool
sameClick = do
	pressed <- Mouse.down
	pressed2 <- Mouse.down
	pure $ pressed == pressed2

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b0 = do b <- Mouse.down
		bool (clickOn b0) (pure ()) (b == b0)

leftClick, middleClick, rightClick :: React s (Singleton Mouse.Down) ()
[leftClick, middleClick, rightClick] = clickOn
	<$> [Mouse.ButtonPrimary, Mouse.ButtonMiddle, Mouse.ButtonSecondary]

releaseOn :: Mouse.Button -> React s (Singleton Mouse.Up) ()
releaseOn b0 = do
	b <- Mouse.up
	bool (releaseOn b0) (pure ()) (b == b0)

leftUp :: React s (Singleton Mouse.Up) ()
leftUp = releaseOn Mouse.ButtonPrimary

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = l `first` r >>= \case L _ -> pure True; _ -> pure False

doubler :: React s (TryWait :- Singleton Mouse.Down) ()
doubler = adjust rightClick
	>> (bool doubler (pure ()) =<< rightClick `before` sleep 0.2)

data BoxesState = BoxesState

curRect :: Point -> Sig s (Singleton Mouse.Move) Rect ()
curRect p1 = Rect p1 <$%> Mouse.position

boxesToView :: [Box] -> V.View
boxesToView = V.View . (boxToView1 <$>)

boxToView1 :: Box -> V.View1
boxToView1 (Box (Rect lu rd) c) = V.Box lu rd $ bColorToColor c

boxToView :: Box -> V.View
boxToView (Box (Rect lu rd) c) = V.View [V.Box lu rd $ bColorToColor c]

bColorToColor :: BColor -> Rgb Double
bColorToColor = fromJust . \case
	Red -> rgbDouble 0.8 0.1 0.05
	Green -> rgbDouble 0.2 0.6 0.1
	Blue -> rgbDouble 0.2 0.2 0.8
	Yellow -> rgbDouble 0.8 0.7 0.1
	Cyan -> rgbDouble 0.2 0.6 0.6
	Magenta -> rgbDouble 0.5 0.2 0.4

rectToView :: Rect -> V.View
rectToView (Rect lu rd) = V.View [V.Box lu rd . fromJust $ rgbDouble 0.2 0.6 0.1]

-- data Rect = Rect { leftUp :: Point, rightdown :: Point }
data Rect = Rect Point Point deriving Show

type Point = (Double, Double)

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect r
wiggleRect (Rect lu rd) = rectAtTime <$%> elapsed
	where
	rectAtTime t = Rect (A.first (+ dx t) lu) (A.first (+ dx t) rd)
	dx t = sin (realToFrac t * 5) * 15

drClickOn :: Rect -> React s (Mouse.Down :- Mouse.Move :- TryWait :- 'Nil) ()
drClickOn rct = void . find (`inside` rct) . (fst <$%>) $ Mouse.position `indexBy` repeat doubler
	where (x, y) `inside` Rect (l, u) (r, d) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)

defineRect :: Sig s (Mouse.Move :- Mouse.Down :- Mouse.Up :- 'Nil) Rect Rect
defineRect = either error pure <=< runExceptT
	$ ExceptT . adjustSig . completeRect =<< ExceptT (waitFor $ adjust firstPoint)

firstPoint :: React s (Mouse.Down :- Mouse.Move :- 'Nil) (Either String Point)
firstPoint = (<$> Mouse.position `at` leftClick)
	$ const (neverOccur "firstPoint 1") `either` (maybe (neverOccur "firstPoint 2") Right . fst)

completeRect :: Point -> Sig s (Mouse.Up :- Mouse.Move :- 'Nil) Rect (Either String Rect)
completeRect p1 = (<$> (Rect p1 <$%> Mouse.position) `until` leftUp)
	$ const (neverOccur "never occur") `either` (Right . fst)

neverOccur :: String -> Either String a
neverOccur msg = Left $ "never occur: " ++ msg

data Box = Box Rect BColor deriving Show
data BColor = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

cycleColor :: Sig s (Mouse.Down :- 'Nil) BColor ()
cycleColor = go . cycle $ fromList [Red .. Magenta] where
	go (h :~ t) = emit h >>
		(bool (pure ()) (go t)
			=<< waitFor (middleClick `before` rightClick))

chooseBoxColor :: Rect -> Sig s (Mouse.Down :- DeltaTime :- 'Nil) Box ()
chooseBoxColor r = Box <$%> adjustSig (wiggleRect r) <*%> adjustSig cycleColor

box :: Sig s (Mouse.Move :- Mouse.Down :- Mouse.Up :- DeltaTime :- TryWait :- 'Nil) Box ()
box = (`Box` Red) <$%> adjustSig defineRect
	>>= (>>) <$> adjustSig . chooseBoxColor <*> waitFor . adjust . drClickOn

boxes :: Sig s (Mouse.Move :- Mouse.Down :- Mouse.Up :- DeltaTime :- TryWait :- 'Nil) [Box] ()
boxes = () <$ parList (spawn box)
