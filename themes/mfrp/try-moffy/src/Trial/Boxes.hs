{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds, PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes (
	-- * Sig boxes
	boxes,
	-- * OTHERS
	doubler, drClickOn, cycleColor, wiggleRect, firstPoint, completeRect, defineRect,
	chooseBoxColor, box
	) where

import Prelude hiding (repeat, cycle, scanl, until)

import Control.Monad (void, (<=<))
import Control.Monad.Trans.Except (pattern ExceptT, runExceptT)
import Control.Moffy (
	Sig, React, Firstable, adjust, adjustSig, emit, waitFor, repeat, find,
	first, at, until, indexBy, spawn, parList )
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow (
	MouseDown, MouseUp, MouseMove, Point,
	leftClick, middleClick, rightClick, leftUp, mousePos )
import Control.Moffy.Event.Time (DeltaTime, TryWait, elapsed, sleep)
import Data.Type.Set (pattern Nil, Singleton, (:-), (:+:))
import Data.Type.Flip ((<$%>), (<*%>))
import Data.Bool (bool)
import Data.List.NonEmpty (fromList)
import Data.List.Infinite (Infinite(..), cycle)
import Data.Or (Or(..))

import qualified Control.Arrow as Arr (first)

import Control.Moffy.Viewable.Shape (Box(..), Rect(..), BColor(..))
import Trial.Boxes.BoxEv (SigB)

---------------------------------------------------------------------------

-- * BOXES
-- * DEFINE RECT
-- * CHOOSE BOX COLOR
-- * DR CLICK ON
-- * BEFORE

---------------------------------------------------------------------------
-- BOXES
---------------------------------------------------------------------------

boxes :: SigB s [Box] ()
boxes = () <$ parList (spawn box)

box :: SigB s Box ()
box = (`Box` Red) <$%> adjustSig defineRect
	>>= (>>) <$> adjustSig . chooseBoxColor <*> waitFor . adjust . drClickOn

---------------------------------------------------------------------------
-- DEFINE RECT
---------------------------------------------------------------------------

defineRect :: Sig s (LoadDefaultWindow :- MouseDown :- MouseUp :- MouseMove :- 'Nil) Rect Rect
defineRect = either error pure <=< runExceptT
	$ ExceptT . adjustSig . completeRect
		=<< ExceptT (waitFor $ adjust firstPoint)

firstPoint :: React s (LoadDefaultWindow :- MouseDown :- MouseMove :- 'Nil) (Either String Point)
firstPoint = (<$> mousePos `at` leftClick)
	$ const (neverOccur "firstPoint 1") `either` (maybe (neverOccur "firstPoint 2") Right . fst)

completeRect ::
	Point -> Sig s (LoadDefaultWindow :- MouseUp :- MouseMove :- 'Nil) Rect (Either String Rect)
completeRect p1 = (<$> (Rect p1 <$%> mousePos) `until` leftUp)
	$ const (neverOccur "completeRect") `either` (Right . fst)

neverOccur :: String -> Either String a
neverOccur msg = Left $ "never occur: " ++ msg

---------------------------------------------------------------------------
-- CHOOSE BOX COLOR
---------------------------------------------------------------------------

chooseBoxColor :: Rect -> Sig s (LoadDefaultWindow :- MouseDown :- DeltaTime :- 'Nil) Box ()
chooseBoxColor r = Box <$%> adjustSig (wiggleRect r) <*%> adjustSig cycleColor

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect ()
wiggleRect (Rect lu rd) = (<$%> elapsed) \t -> let
	dx = (sin (fromRational (toRational t) * 5) * 15 :: Double) in
	Rect ((+ dx) `Arr.first` lu) ((+ dx) `Arr.first` rd)

cycleColor :: Sig s (LoadDefaultWindow :- MouseDown :- 'Nil) BColor ()
cycleColor = go . cycle $ fromList [Red .. Magenta] where
	go (h :~ t) = emit h >>
		(bool (pure ()) (go t)
			=<< waitFor (middleClick `before` rightClick))

---------------------------------------------------------------------------
-- DR CLICK ON
---------------------------------------------------------------------------

drClickOn :: Rect -> React s (LoadDefaultWindow :- MouseDown :- MouseMove :- TryWait :- 'Nil) ()
drClickOn rct = void . find (`inside` rct) $ mousePos `indexBy` repeat doubler
	where (x, y) `inside` Rect (l, u) (r, d) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)

doubler :: React s (LoadDefaultWindow :- MouseDown :- TryWait :- 'Nil) ()
doubler = adjust rightClick
	>> (bool doubler (pure ()) =<< rightClick `before` sleep 0.2)

---------------------------------------------------------------------------
-- BEFORE
---------------------------------------------------------------------------

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = l `first` r >>= \case L _ -> pure True; _ -> pure False
