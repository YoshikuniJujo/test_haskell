{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes (Box(..), Rect(..), Color(..), boxes, doubler) where

import Prelude hiding (repeat, cycle, scanl, until)

import Control.Moffy
import Control.Moffy.Event.Mouse
import Data.Type.Set
import Data.Type.Flip
import Data.Bool
import Data.Maybe
import Data.List.NonEmpty hiding (cycle, repeat, scanl, break)
import Data.List.Infinite hiding (repeat, scanl)
import Data.Or
import Data.Time

import qualified Control.Arrow as Arr

import Trial.Boxes.Box
import Trial.Boxes.Event

curRect :: Point -> Sig s (MouseMove :- 'Nil) Rect ()
curRect p1 = Rect p1 <$%> mousePos

elapsed :: Sig s (Singleton DeltaTime) DiffTime ()
elapsed = scanl (+) 0 (repeat deltaTime)

inside :: Point -> Rect -> Bool
(x, y) `inside` Rect (l, u) (r, d) =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= d || d <= y && y <= u)

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = l `first` r >>= \case L _ -> pure True; _ -> pure False

firstPoint :: React s (MouseDown :- MouseMove :- 'Nil) (Maybe Point)
firstPoint = (<$> mousePos `at` leftClick)
	\case Left () -> Nothing; Right (p, ()) -> p

completeRect :: Point -> Sig s (MouseUp :- MouseMove :- 'Nil) Rect (Maybe Rect)
completeRect p1 =
	either (const Nothing) (Just . fst) <$> curRect p1 `until` leftUp

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect ()
wiggleRect (Rect lu rd) = (<$%> elapsed) \t -> let
	dx = round (sin (fromRational (toRational t) * 5) * 15 :: Double) in
	Rect ((+ dx) `Arr.first` lu) ((+ dx) `Arr.first` rd)

cycleColor :: Sig s (Singleton MouseDown) Color ()
cycleColor = cc . cycle $ fromList [Red .. Magenta] where
	cc (h :~ t) = emit h >>
		(bool (pure ()) (cc t)
			=<< waitFor (middleClick `before` rightClick))

posInside :: Rect -> Sig s es Point y -> React s es (Either Point y)
posInside rct = find (`inside` rct)

mousePos :: Sig s (Singleton MouseMove) Point ()
mousePos = repeat mouseMove

doubler :: React s (MouseDown :- TryWait :- 'Nil) ()
doubler = do
	adjust rightClick
	bool doubler (pure ()) =<< (rightClick `before` sleep 0.2)

defineRect :: Sig s (MouseDown :- MouseUp :- MouseMove :- 'Nil) Rect Rect
defineRect = waitFor (adjust firstPoint) >>= \case
	Nothing -> error "never occur"
	Just p1 -> fromMaybe (error "never occur") <$> adjustSig (completeRect p1)

chooseBoxColor :: Rect -> Sig s (MouseDown :- DeltaTime :- 'Nil) Box ()
chooseBoxColor r = Box <$%> adjustSig (wiggleRect r) <*%> adjustSig cycleColor

drClickOn :: Rect -> React s (MouseDown :- MouseMove :- TryWait :- 'Nil) (Either Point (Either () (Maybe Point, ())))
drClickOn rct = posInside rct $ mousePos `indexBy` repeat doubler

box :: SigG s Box ()
box = (() <$) $ (`Box` Red) <$%> adjustSig defineRect >>= \r ->
	adjustSig (chooseBoxColor r) >> waitFor (adjust $ drClickOn r)

newBoxes :: SigG s (ISigG s Box ()) ()
newBoxes = spawn box

boxes :: SigG s [Box] ()
boxes = () <$ parList newBoxes
