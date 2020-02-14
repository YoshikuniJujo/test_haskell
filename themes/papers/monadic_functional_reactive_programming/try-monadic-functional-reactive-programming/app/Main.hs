{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Set

import React
import MouseAndTime
import GuiEv

import Field
import ButtonEvent

main :: IO ()
main = do
	f <- openField "クリック" [exposureMask, buttonPressMask]
	interpret (handle f) mouseDown >>= print
	closeField f

handle :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handle f r = withNextEvent f \e -> case e of
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {	buttonNumber = Button1,
				pressOrRelease = Press,
				position = (_x, _y) } -> do
			pure . singleton . MouseDown $ Occurred [MLeft]
		_ -> handle f r
	_ -> handle f r
