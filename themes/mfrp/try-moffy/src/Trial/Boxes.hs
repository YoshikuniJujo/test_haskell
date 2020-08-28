{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds, PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes (
	-- * Sig boxes
	boxes ) where

import Prelude hiding (repeat, cycle, scanl, until)

import Control.Monad (void, (<=<))
import Control.Monad.Trans.Except (pattern ExceptT, runExceptT)
import Control.Moffy (
	Sig, React, Firstable, adjust, adjustSig, emit, waitFor, repeat, find,
	first, at, until, indexBy, spawn, parList )
import Control.Moffy.Event.Mouse (
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

import Trial.Boxes.Box (Box(..), Rect(..), Color(..))
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

defineRect :: Sig s (MouseDown :- MouseUp :- MouseMove :- 'Nil) Rect Rect
defineRect = either error pure <=< runExceptT
	$ ExceptT . adjustSig . completeRect
		=<< ExceptT (waitFor $ adjust firstPoint)

firstPoint :: React s (MouseDown :- MouseMove :- 'Nil) (Either String Point)
firstPoint = (<$> mousePos `at` leftClick)
	$ const neverOccur `either` (maybe neverOccur Right . fst)

completeRect ::
	Point -> Sig s (MouseUp :- MouseMove :- 'Nil) Rect (Either String Rect)
completeRect p1 = (<$> (Rect p1 <$%> mousePos) `until` leftUp)
	$ const neverOccur `either` (Right . fst)

neverOccur :: Either String a
neverOccur = Left "never occur"

---------------------------------------------------------------------------
-- CHOOSE BOX COLOR
---------------------------------------------------------------------------

chooseBoxColor :: Rect -> Sig s (MouseDown :- DeltaTime :- 'Nil) Box ()
chooseBoxColor r = Box <$%> adjustSig (wiggleRect r) <*%> adjustSig cycleColor

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect ()
wiggleRect (Rect lu rd) = (<$%> elapsed) \t -> let
	dx = round (sin (fromRational (toRational t) * 5) * 15 :: Double) in
	Rect ((+ dx) `Arr.first` lu) ((+ dx) `Arr.first` rd)

cycleColor :: Sig s (Singleton MouseDown) Color ()
cycleColor = go . cycle $ fromList [Red .. Magenta] where
	go (h :~ t) = emit h >>
		(bool (pure ()) (go t)
			=<< waitFor (middleClick `before` rightClick))

---------------------------------------------------------------------------
-- DR CLICK ON
---------------------------------------------------------------------------

drClickOn :: Rect -> React s (MouseDown :- MouseMove :- TryWait :- 'Nil) ()
drClickOn rct = void . find (`inside` rct) $ mousePos `indexBy` repeat doubler
	where (x, y) `inside` Rect (l, u) (r, d) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)

doubler :: React s (MouseDown :- TryWait :- 'Nil) ()
doubler = adjust rightClick
	>> (bool doubler (pure ()) =<< rightClick `before` sleep 0.2)

---------------------------------------------------------------------------
-- BEFORE
---------------------------------------------------------------------------

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = l `first` r >>= \case L _ -> pure True; _ -> pure False
