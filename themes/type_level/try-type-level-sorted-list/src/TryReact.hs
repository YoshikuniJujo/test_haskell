{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryReact where

import Sorted
import OpenUnionValue
import React
import Field

tryMouseDown :: IO ()
tryMouseDown = do
	f <- openField "tryMouseDown" [exposureMask, buttonPressMask]
	interpret (handleMouseDown f) mouseDown >>= print
	closeField f

handleMouseDown :: Field -> EvReqs (Singleton MouseDown) -> IO [EvOcc (Singleton MouseDown)]
handleMouseDown f reqs = withNextEvent f \case
	ExposeEvent {} -> flushField f >> handleMouseDown f reqs
	ev@ButtonEvent {
		ev_button = b
		} -> print ev >> print b >> pure [inj $ OccurredMouseDown [button b]]
	ev -> print ev >> handleMouseDown f reqs

trySameClick :: IO ()
trySameClick = do
	f <- openField "trySameClick" [exposureMask, buttonPressMask]
	interpret (handleMouseDown f) sameClick >>= print
	closeField f

button :: Button -> MouseBtn
button 1 = MLeft
button 2 = MMiddle
button 3 = MRight
button 4 = MUp
button 5 = MDown
button _ = error "Unknown button"

tryLeftClick :: IO ()
tryLeftClick = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask]
	interpret (handleMouseDown f) leftClick
	closeField f
