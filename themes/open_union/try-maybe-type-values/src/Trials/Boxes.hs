{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes (boxes) where

import Prelude hiding (repeat, cycle, scanl, until, break)

import Data.Type.Set ((:+:))
import Data.Type.Flip ((<$%>), (<*%>))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Or (Or(..))
import Data.List.NonEmpty (fromList)
import Data.List.Infinite (Infinite(..), cycle)
import Data.Time (DiffTime)

import qualified Control.Arrow as Arr

import MonadicFrp (
	React, Firstable,
	adjust, first, emit, waitFor, scanl, find, repeat, spawn, parList,
	at, break, until, indexBy )
import Trials.Boxes.View (Box(..), Rect(..), Color(..))
import Trials.Boxes.Event (
	SigG, ReactG, Point, sleep, deltaTime,
	leftClick, middleClick, rightClick, leftUp, mouseMove, deleteEvent )

---------------------------------------------------------------------------

before :: Firstable es es' => React es a -> React es' b -> React (es :+: es') Bool
l `before` r = (<$> l `first` r) \case L _ -> True; _ -> False

doubler :: ReactG ()
doubler = adjust rightClick >>
	(bool doubler (pure ()) =<< adjust (rightClick `before` sleep 0.2))

cycleColor :: SigG Color ()
cycleColor = cc . cycle $ fromList [Red .. Magenta] where
	cc (h :~ t) = emit h >>
		(bool (pure ()) (cc t)
			=<< waitFor (adjust $ middleClick `before` rightClick))

mousePos :: SigG Point ()
mousePos = repeat $ adjust mouseMove

curRect :: Point -> SigG Rect ()
curRect p1 = Rect p1 <$%> mousePos

elapsed :: SigG DiffTime ()
elapsed = scanl (+) 0 . repeat $ adjust deltaTime

wiggleRect :: Rect -> SigG Rect ()
wiggleRect (Rect lu rd) = (<$%> elapsed) \t -> let
	dx = round (sin (fromRational (toRational t) * 5) * 15 :: Double) in
	Rect ((+ dx) `Arr.first` lu) ((+ dx) `Arr.first` rd)

firstPoint :: ReactG (Maybe Point)
firstPoint = either (const Nothing) (Just . fst) <$> mousePos `at` leftClick

completeRect :: Point -> SigG Rect (Maybe Rect)
completeRect p1 =
	either (const Nothing) (either Just (const Nothing) . fst) <$> curRect p1 `until` leftUp

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Nothing -> error "never occur 1"
	Just p1 -> fromMaybe (error "never occur 2") <$> completeRect p1

chooseBoxColor :: Rect -> SigG Box ()
chooseBoxColor r = Box <$%> wiggleRect r <*%> cycleColor

drClickOn :: Rect -> ReactG ()
drClickOn rct = () <$ find (`inside` rct) (mousePos `indexBy` repeat doubler)
	where (x, y) `inside` Rect (l, u) (r, d) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)

box :: SigG Box ()
box = (`Box` Red) <$%> defineRect >>= \r ->
	chooseBoxColor r >> waitFor (drClickOn r)

boxes :: SigG [Box] ()
boxes = () <$ parList (spawn box) `break` deleteEvent
