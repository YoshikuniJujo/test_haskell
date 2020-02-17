{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Set

import React
import GuiEv
import ColoredBoxes

import Field
import ButtonEvent

main :: IO ()
main = do
	f <- openField "おなじボタン" [exposureMask, buttonPressMask]
	interpret (handle f) sameClick >>= print
	closeField f

handle :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handle f r = withNextEvent f \e -> case e of
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = bn,
			pressOrRelease = Press,
			position = (_x, _y) } -> case button bn of
				Just m -> pure . singleton . MouseDown $ Occurred [m]
				Nothing -> handle f r
		_ -> handle f r
	_ -> print e >> handle f r

button :: ButtonNumber -> Maybe MouseBtn
button Button1 = Just MLeft
button Button2 = Just MMiddle
button Button3 = Just MRight
button _ = Nothing
