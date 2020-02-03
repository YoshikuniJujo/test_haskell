{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Set
import System.Exit

import Field
import ButtonEvent
import MonadicFrp

main :: IO ()
main = do
	f <- openField "長方形をひとつ" [
--		exposureMask, buttonPressMask, buttonReleaseMask, button1MotionMask ]
		exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask ]
	_ <- interpretSig (handle f) (fillRect' f) defineRect
	closeField f

handle :: Field -> EvReqs GUIEv -> IO (EvOccs GUIEv)
handle f r = withNextEvent f \case
	DestroyWindowEvent {} -> closeField f >> exitSuccess
	ExposeEvent {} -> flushField f >> handle f r
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {	buttonNumber = Button1,
				pressOrRelease = Press } -> pure . singleton . MouseDown $ Occurred [MLeft]
		Just BtnEvent {	buttonNumber = Button1,
				pressOrRelease = Release } -> pure . singleton . MouseUp $ Occurred [MLeft]
		e -> print e >> handle f r
	ev@MotionEvent {} -> case buttonEvent ev of
		Just BtnEvent {	buttonNumber = ButtonX,
				pressOrRelease = Move,
				position = (x, y) } -> do
			pure . singleton. MouseMove $ Occurred (fromIntegral x, fromIntegral y)
		e -> print e >> handle f r
	ev	| isDeleteEvent f ev -> destroyField f >> handle f r
		| otherwise -> print ev >> handle f r

fillRect' :: Field -> Rect -> IO ()
fillRect' f (Rect (l, t) (r, u)) = do
	clearField f
	fillRect f 0xff0000
		(round $ min l r) (round $ min t u)
		(round . abs $ r - l) (round . abs $ u - t)
	flushField f
