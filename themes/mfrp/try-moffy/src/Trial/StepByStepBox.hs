{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox where

import Prelude hiding (cycle, repeat, scanl, until, break)
import qualified Prelude as P

import Control.Monad.State
import Control.Moffy
import Control.Moffy.Handle hiding (before)
import Control.Moffy.Run
import Data.Type.Set
import Data.Type.Flip
import Data.Bool
import Data.Maybe
import Data.Or
import Data.List.NonEmpty hiding (cycle, repeat, scanl, break)
import Data.List.Infinite hiding (repeat, scanl)
import Data.Time
import Data.Time.Clock.System

import qualified Control.Arrow as Arr

import Control.Moffy.Event.Delete
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle.XField
import Field hiding (Point)

import Trial.Boxes.Event
import Trial.Boxes.Handle

tryClick :: IO [MouseBtn]
tryClick = do
	f <- openField "TRY CLICK" [buttonPressMask, exposureMask]
	interpretReact (retry $ handle Nothing f) mouseDown <* closeField f

sameClick :: React s (Singleton MouseDown) Bool
sameClick = (==) <$> mouseDown <*> mouseDown

trySameClick :: IO Bool
trySameClick = do
	f <- openField "TRY SAME CLICK" [buttonPressMask]
	interpretReact (retry $ handle Nothing f) sameClick <* closeField f

leftDownRightUp :: React s (MouseDown :- MouseUp :- 'Nil) (Or () ())
leftDownRightUp = leftClick `first` rightUp

tryLeftDownRightUp :: IO (Or () ())
tryLeftDownRightUp = do
	f <- openField "LEFT DOWN RIGHT UP" [buttonPressMask, buttonReleaseMask]
	interpretReact (retry $ handle Nothing f) leftDownRightUp <* closeField f

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = l `first` r >>= \case L _ -> pure True; _ -> pure False

doubler :: React s (MouseDown :- TryWait :- 'Nil) ()
doubler = do
	adjust rightClick
	bool doubler (pure ()) =<< (rightClick `before` sleep 0.2)

tryDoubler :: IO ()
tryDoubler = do
	f <- openField "TRY DOUBLER" [buttonPressMask, exposureMask]
	void . (interpretReactSt (handleBoxes 0.05 f) InitMode doubler `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

cycleColor :: Sig s (Singleton MouseDown) Color ()
cycleColor = cc . cycle $ fromList [Red .. Magenta] where
	cc (h :~ t) = emit h >>
		(bool (pure ()) (cc t)
			=<< waitFor (middleClick `before` rightClick))

tryCycleColor :: IO ()
tryCycleColor = do
	f <- openField "TRY CYCLE COLOR" [buttonPressMask, exposureMask]
	void . (interpretSt (handleBoxes 0.05 f) InitMode (liftIO . print) cycleColor `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f

mousePos :: Sig s (Singleton MouseMove) Point ()
mousePos = repeat mouseMove

tryMousePos :: IO ()
tryMousePos = do
	f <- openField "TRY MOUSE POS" [pointerMotionMask, exposureMask]
	void . (interpretSt (handleBoxes 0.05 f) InitMode (liftIO . print) mousePos `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f

curRect :: Point -> Sig s (MouseMove :- 'Nil) Rect ()
curRect p1 = Rect p1 <$%> mousePos

tryCurRect :: IO ()
tryCurRect = trySigGRect "TRY CUR RECT" $ curRect (200, 150)

data Rect = Rect { leftup :: Point, rightdown :: Point  } deriving Show

tryReactG :: Adjustable es (KeyEv :+: GuiEv) => String -> React s es r -> IO r
tryReactG ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	((r, _), _) <- (interpretReactSt (handleBoxes 0.05 f) InitMode sig `runStateT`)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f

trySigGRect :: Adjustable es (KeyEv :+: GuiEv) => String -> Sig s es Rect r -> IO r
trySigGRect ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	(r, _) <- (interpretSt (handleBoxes 0.05 f) InitMode
				(liftIO . withFlush f . drawRect f (colorToPixel Red)) sig `runStateT`)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f

withFlush :: Field -> IO a -> IO a
withFlush f act = clearField f >> act <* flushField f

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	[l, u] = fromIntegral <$> [l_ `min` r_, u_ `min` d_]
	[w, h] = fromIntegral <$> [abs $ r_ - l_, abs $ d_ - u_]


colorToPixel :: Color -> Pixel
colorToPixel = \case
	Red -> 0xff0000; Green -> 0x00ff00; Blue -> 0x0000ff
	Yellow -> 0xffff00; Cyan -> 0xff00ff; Magenta -> 0x00ffff

elapsed :: Sig s (Singleton DeltaTime) DiffTime ()
elapsed = scanl (+) 0 (repeat deltaTime)

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect ()
wiggleRect (Rect lu rd) = (<$%> elapsed) \t -> let
	dx = round (sin (fromRational (toRational t) * 5) * 15 :: Double) in
	Rect ((+ dx) `Arr.first` lu) ((+ dx) `Arr.first` rd)

tryWiggleRect :: IO ()
tryWiggleRect = trySigGRect "TRY WIGGLE RECT" . wiggleRect $ Rect (200, 150) (400, 300)

posInside :: Rect -> Sig s es Point y -> React s es (Either Point y)
posInside rct = find (`inside` rct)

inside :: Point -> Rect -> Bool
(x, y) `inside` Rect (l, u) (r, d) =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= d || d <= y && y <= u)

tryPosInside :: IO (Either Point ())
tryPosInside = do
	f <- openField "TRY POS INSIDE" [pointerMotionMask, exposureMask]
	drawRect f (colorToPixel Red) $ Rect (200, 150) (400, 300)
	((r, _), _) <- (interpretReactSt (handleBoxes 0.05 f) InitMode
			(posInside (Rect (200, 150) (400, 300)) mousePos) `runStateT`)
		. systemToTAITime =<< getSystemTime
	r <$ closeField f

firstPoint :: React s (MouseDown :- MouseMove :- 'Nil) (Maybe Point)
firstPoint = (<$> mousePos `at` leftClick)
	\case Left () -> Nothing; Right (p, ()) -> Just p

tryFirstPoint :: IO (Maybe Point)
tryFirstPoint = tryReactG "TRY FIRST POINT" firstPoint

completeRect :: Point -> Sig s (MouseUp :- MouseMove :- 'Nil) Rect (Maybe Rect)
completeRect p1 =
	either (const Nothing) (Just . fst) <$> curRect p1 `until` leftUp

tryCompleteRect :: IO (Maybe Rect)
tryCompleteRect = trySigGRect "TRY COMPLETE RECT" $ completeRect (200, 150)

defineRect :: Sig s (MouseDown :- MouseUp :- MouseMove :- 'Nil) Rect Rect
defineRect = waitFor (adjust firstPoint) >>= \case
	Nothing -> error "never occur"
	Just p1 -> fromMaybe (error "never occur") <$> adjustSig (completeRect p1)

tryDefineRect :: IO Rect
tryDefineRect = trySigGRect "TRY DEFINE RECT" defineRect

chooseBoxColor :: Rect -> Sig s (MouseDown :- DeltaTime :- 'Nil) Box ()
chooseBoxColor r = Box <$%> adjustSig (wiggleRect r) <*%> adjustSig cycleColor

data Box = Box Rect Color deriving Show

tryChooseBoxColor :: IO ()
tryChooseBoxColor = trySigGBox "TRY CHOOSE BOX COLOR" . chooseBoxColor $ Rect (200, 150) (400, 300)

trySigGBox :: Adjustable es (KeyEv :+: GuiEv) => String -> Sig s es Box r -> IO r
trySigGBox ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	(r, _) <- (interpretSt (handleBoxes 0.05 f) InitMode
				(liftIO . withFlush f . drawBox f) sig `runStateT`)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f

drawBox :: Field -> Box -> IO ()
drawBox f (Box rct clr) = drawRect f (colorToPixel clr) rct

drClickOn :: Rect -> React s (MouseDown :- MouseMove :- TryWait :- 'Nil) (Either Point (Either () (Maybe Point, ())))
drClickOn rct = posInside rct $ mousePos `indexBy` repeat doubler

tryDrClickOn :: IO (Either Point (Either () (Maybe Point, ())))
tryDrClickOn = tryReactG "TRY DOUBLE RIGHT CLICK ON" (drClickOn $ Rect (200, 150) (400, 300))

box :: SigG s Box ()
box = (() <$) $ (`Box` Red) <$%> adjustSig defineRect >>= \r ->
	adjustSig (chooseBoxColor r) >> waitFor (adjust $ drClickOn r)

tryBox :: IO ()
tryBox = trySigGBox "TRY BOX" box

newBoxes :: SigG s (ISigG s Box ()) ()
newBoxes = spawn box

boxes :: SigG s [Box] ()
boxes = () <$ parList newBoxes

trySigGBoxes :: String -> SigG s [Box] r -> IO r
trySigGBoxes ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	(r, _) <- (interpretSt (handleBoxes 0.05 f) InitMode
				(liftIO . drawBoxes f) sig `runStateT`)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f

drawBoxes :: Field -> [Box] -> IO ()
drawBoxes f = withFlush f . (drawBox f `mapM_`) . P.reverse

tryBoxes :: IO ()
tryBoxes = (() <$) . trySigGBoxes "TRY BOXES" $ boxes `break` deleteEvent
