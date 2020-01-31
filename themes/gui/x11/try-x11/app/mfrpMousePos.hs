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
	f <- openField "マウスの位置" [exposureMask, buttonPressMask, button1MotionMask]
	_ <- interpretSig (handle f) print mousePos
	closeField f

handle :: Field -> EvReqs GUIEv -> IO (EvOccs GUIEv)
handle f r = withNextEvent f \case
	DestroyWindowEvent {} -> closeField f >> exitSuccess
	ExposeEvent {} -> flushField f >> handle f r
	ev@MotionEvent {} -> case buttonEvent ev of
		Just BtnEvent {	buttonNumber = ButtonX,
				pressOrRelease = Move,
				position = (x, y) } -> do
			pure . singleton . MouseMove $ Occurred (fromIntegral x, fromIntegral y)
		e -> print e >> handle f r
	ev	| isDeleteEvent f ev -> destroyField f >> handle f r
		| otherwise -> print ev >> handle f r
